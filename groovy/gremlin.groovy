//
// Copyright 2012 James Thornton (http://jamesthornton.com)
// BSD License (see LICENSE for details)
//

// TODO: This will error for property values that are lists.
//       See https://groups.google.com/forum/#!topic/neo4j/sjH2f5dulTQ

// Model - Vertex
//

// Special one-off method for loading UserProfile data more efficiently
// This is a stopgap measure until a better method is devised (perhaps
// using the newer Tree() pipe.
def get_user_profile_data(user_id) {
  
  // ARGH, gross code dup - fix with some magic somewhere.
  def create_indexed_vertex(data,index_name,keys) {
    neo4j = g.getRawGraph()
    manager = neo4j.index()
    g.setMaxBufferSize(0)
    g.startTransaction()
    try {
      index = manager.forNodes(index_name)
      vertex = neo4j.createNode()
      for (entry in data.entrySet()) {
        if (entry.value == null) continue;
        vertex.setProperty(entry.key,entry.value)
        if (keys == null || keys.contains(entry.key))
    index.add(vertex,entry.key,String.valueOf(entry.value))
      }
      g.stopTransaction(TransactionalGraph.Conclusion.SUCCESS)
      return vertex
    } catch (e) {
      g.stopTransaction(TransactionalGraph.Conclusion.FAILURE)  
      return e
    }
  }

  t= new Table()
  try {
    item = g.idx("userprofile").get("user_id", Neo4jTokens.QUERY_HEADER + user_id).iterator().next()
    g.v(item.id)._()
      .out('hasCollection').as('vc')
      .table(t,['vc']){[
        "item": it._(),
        //"collections": it._().out('contains')._()
        // Do this if we ever get parsing both the edges and the relationships to work
        // As it is getting the results through lift-json is weirdly tricky...
        "collections": it._().outE('contains').as("e").inV.as("v").table(new Table(), ["e","v"]).cap
        //"collections": it._().outE('contains').as("edges").inV.as("vertices").table(new Table(), ["edges", "vertices"])._()
      ]}.iterate();
  } catch (NoSuchElementException e) {
    item = create_indexed_vertex(["element_type":"userprofile","user_id":user_id], "userprofile", null)  
  }
  ["item": item._(), "virtualcollections": t]
}


// TODO: Fix code duplication with the update equivilent...
def create_indexed_vertex_with_subordinates(data, index_name, subs) {
  import org.neo4j.graphdb.DynamicRelationshipType;
  def neo4j = g.getRawGraph()
  manager = neo4j.index()
  g.setMaxBufferSize(0)
  g.startTransaction()

  def update_vertex(idx, v, data) {
    index = manager.forNodes(idx)
    index.remove(v)
    for (String key in v.getPropertyKeys()) {
      v.removeProperty(key)
    }
    for (entry in data.entrySet()) {
      if (entry.value == null) continue;
      v.setProperty(entry.key,entry.value)
      index.add(v,entry.key,String.valueOf(entry.value))
    }
    return v
  }

  try {
    def vertex = neo4j.createNode()
    update_vertex(index_name, vertex, data)
    for (sub in subs.entrySet()) {
      def label = sub.key
      def relationshipType = DynamicRelationshipType.withName(label)
      def ridx = manager.forRelationships(label)
      def current = g.v(vertex.id).in(label).collect{neo4j.getNodeById(it.id)}
      def sublist = sub.value

      // update the existing ones...
      [current, sublist].transpose().collect {
        def (existing, subdata) = it
        update_vertex(subdata.getAt("index_name"), existing, subdata.getAt("data"))
      }
      // remove any that've been deleted...
      for (i = sublist.size; i < current.size; i++)
        g.removeVertex(g.v(current[i].id))
      // add any that are new...
      for (i = current.size; i < sublist.size; i++) {
        n = neo4j.createNode()
        update_vertex(sublist[i].getAt("index_name"), n, sublist[i].getAt("data"))
        n.createRelationshipTo(vertex, relationshipType)
      }
    }

    g.stopTransaction(TransactionalGraph.Conclusion.SUCCESS)
    return vertex 
  } catch (e) {
    g.stopTransaction(TransactionalGraph.Conclusion.FAILURE)
    return e
  }
}


def create_indexed_vertex(data,index_name,keys) {
  neo4j = g.getRawGraph()
  manager = neo4j.index()
  g.setMaxBufferSize(0)
  g.startTransaction()
  try {
    index = manager.forNodes(index_name)
    vertex = neo4j.createNode()
    for (entry in data.entrySet()) {
      if (entry.value == null) continue;
      vertex.setProperty(entry.key,entry.value)
      if (keys == null || keys.contains(entry.key))
	index.add(vertex,entry.key,String.valueOf(entry.value))
    }
    g.stopTransaction(TransactionalGraph.Conclusion.SUCCESS)
    return vertex
  } catch (e) {
    g.stopTransaction(TransactionalGraph.Conclusion.FAILURE)  
    return e
  }
}

// Neo4j requires you delete all adjacent edges first. 
// Blueprints' removeVertex() method does that; the Neo4jServer DELETE URI does not.
def delete_vertex_with_related(_id, outRels, inRels) {
  vertex = g.v(_id)
  neo4j = g.getRawGraph()

  g.setMaxBufferSize(0)
  g.startTransaction()
  try {
    for (outrel in outRels)
      vertex.out(outrel).collect{g.removeVertex(it)}
    for (inrel in inRels)
      vertex.in(inrel).collect{g.removeVertex(it)}
    g.removeVertex(vertex)
    g.stopTransaction(TransactionalGraph.Conclusion.SUCCESS)
    return true 
  } catch (e) {
    g.stopTransaction(TransactionalGraph.Conclusion.FAILURE)
    return e
  }
}


// construct a query - several of these paths can probably
// be optimised, and I'm not sure if the index is being
// used as much as it could be.
def query(index_name, inrels, outrels, filters, high, low, order_by, docount, dodelete) {
  try {
    def opblocks = [
      "exact":   {it, a, v -> it."$a" == v},
      "iexact":  {it, a, v -> it."$a".toLowerCase() == v.toLowerCase()},
      "contains":{it, a, v -> it."$a".matches(".+$v.+")},
      "icontains":{it, a, v -> it."$a".toLowerCase().matches(".+" + v.toLowerCase() + ".+")},
      "startswith":{it, a, v -> it."$a".matches("^$v.+")},
      "endswith":{it, a, v -> it."$a".matches(".+$v\$")},
      "isnull":  {it, a, v -> it."$a" == null},
      "gt":      {it, a, v -> it."$a" >  v},
      "lt":      {it, a, v -> it."$a" <  v},
      "gte":     {it, a, v -> it."$a" >= v},
      "lte":     {it, a, v -> it."$a" <= v},
    ]

    def pipe = g.V._()

    for (relation in inrels) {
      def (relname, outV) = relation
      pipe = pipe.retain(g.v(outV).inE(relname).outV.toList())
    }

    for (relation in outrels) {
      def (relname, outV) = relation
      pipe = pipe.retain(g.v(outV).outE(relname).inV.toList())
    }

    if (index_name != null)
      pipe = pipe.filter{it.element_type==index_name}

    for (filter in filters) {
      def (attr, op, value) = filter

      def opblock = opblocks.get(op)
      if (opblock == null)
        throw new Exception("Unsupported filter operator: '$op'")
      pipe = pipe.filter{opblock(it, attr, value)}
    }
    
    if (!docount && low != null && high != null) {
      if (low != 0 && high == null)
        pipe = pipe.range(low, -1)
      else if (low == 0 && high != null)
        pipe = pipe.range(0, high)
      else if (low > 0 && high != null)
        pipe = pipe.range(low, high)
    }
  
    if (order_by != null && order_by) {
      for (order in order_by) {
        def (attr, desc) = order
        pipe = pipe.sort{it."$attr"}
        if (desc)
          pipe = pipe.reverse()
      }
    }

    if (docount)
      return pipe.count()

    if (dodelete) {
      pipe.collect{g.removeVertex(it)}
      return true;
    } 
    return pipe
  } catch (e) {
    return e
  }
}




/* Update a vertex and all it's subordinate relations.
 * These are vertices with the given outgoing relationships
 * pointing to the parent node:
 *
 * The subs data structure looks like:
 *
 * {
 *    relationName -> [
 *      {
 *        index_name -> 'idxName',
 *        data -> {
 *          prop1 -> val1,
 *          prop2 -> val2
 *        }
 *      },
 *      ... more
 *    ]
 */


def update_indexed_vertex_with_subordinates(_id, data, index_name, subs) {
  import org.neo4j.graphdb.DynamicRelationshipType;
  vertex = g.getRawGraph().getNodeById(_id)
  def neo4j = g.getRawGraph()
  manager = neo4j.index()
  g.setMaxBufferSize(0)
  g.startTransaction()

  def update_vertex(idx, v, data) {
    index = manager.forNodes(idx)
    index.remove(v)
    for (String key in v.getPropertyKeys()) {
      v.removeProperty(key)
    }
    for (entry in data.entrySet()) {
      if (entry.value == null) continue;
      v.setProperty(entry.key,entry.value)
      index.add(v,entry.key,String.valueOf(entry.value))
    }
    return v
  }

  try {
    update_vertex(index_name, vertex, data)
    for (sub in subs.entrySet()) {
      def label = sub.key
      def relationshipType = DynamicRelationshipType.withName(label)
      def ridx = manager.forRelationships(label)
      def current = g.v(_id).in(label).collect{neo4j.getNodeById(it.id)}
      def sublist = sub.value

      // update the existing ones...
      [current, sublist].transpose().collect {
        def (existing, subdata) = it
        update_vertex(subdata.getAt("index_name"), existing, subdata.getAt("data"))
      }
      // remove any that've been deleted...
      for (i = sublist.size; i < current.size; i++)
        g.removeVertex(g.v(current[i].id))
      // add any that are new...
      for (i = current.size; i < sublist.size; i++) {
        n = neo4j.createNode()
        update_vertex(sublist[i].getAt("index_name"), n, sublist[i].getAt("data"))
        n.createRelationshipTo(vertex, relationshipType)
      }
    }

    g.stopTransaction(TransactionalGraph.Conclusion.SUCCESS)
    return vertex 
  } catch (e) {
    g.stopTransaction(TransactionalGraph.Conclusion.FAILURE)
    return e
  }
}

def update_indexed_vertex(_id, data, index_name, keys) {
  vertex = g.getRawGraph().getNodeById(_id)
  manager = g.getRawGraph().index()
  g.setMaxBufferSize(0)
  g.startTransaction()
  try {
    index = manager.forNodes(index_name)
    index.remove(vertex)
    for (String key in vertex.getPropertyKeys())
      vertex.removeProperty(key)
    for (entry in data.entrySet()) {
      if (entry.value == null) continue;
      vertex.setProperty(entry.key,entry.value)
      if (keys == null || keys.contains(entry.key))
	index.add(vertex,entry.key,String.valueOf(entry.value))
    }
    g.stopTransaction(TransactionalGraph.Conclusion.SUCCESS)
    return vertex 
  } catch (e) {
    g.stopTransaction(TransactionalGraph.Conclusion.FAILURE)
    return e
  }
}

// Model - Edge

def create_indexed_edge(outV,label,inV,data,index_name,keys,label_var) {
  import org.neo4j.graphdb.DynamicRelationshipType;
  neo4j = g.getRawGraph()
  manager = neo4j.index()
  vertex = neo4j.getNodeById(outV)
  relationshipType = DynamicRelationshipType.withName(label)
  g.setMaxBufferSize(0)
  g.startTransaction()
  try {
    index = manager.forRelationships(index_name)
    edge = vertex.createRelationshipTo(neo4j.getNodeById(inV),relationshipType)
    for (entry in data.entrySet()) {
      if (entry.value == null) continue;
      edge.setProperty(entry.key,entry.value)
      if (keys == null || keys.contains(entry.key))
	index.add(edge,entry.key,String.valueOf(entry.value))
    }
    index.add(edge,label_var,String.valueOf(label))
    g.stopTransaction(TransactionalGraph.Conclusion.SUCCESS)
    return edge
  } catch (e) {
    g.stopTransaction(TransactionalGraph.Conclusion.FAILURE)
    return e
  }
}

def delete_edge(_id) {
  // TODO... this is more complicated because there
  // can be multiple edges with the same label between
  // a pair of nodes. The only safe way is to delete
  // an edge by its id.
  g.removeEdge(g.e(_id))
}

// don't need to update indexed label, it can't change
def update_indexed_edge(_id, data, index_name, keys) {
  neo4j = g.getRawGraph()
  manager = neo4j.index()
  edge = neo4j.getRelationshipById(_id)
  g.setMaxBufferSize(0)
  g.startTransaction()
  try {
    index = manager.forRelationships(index_name)
    index.remove(edge)
    for (String key in edge.getPropertyKeys())
      edge.removeProperty(key)
    for (entry in data.entrySet()) {
      if (entry.value == null) continue;
      edge.setProperty(entry.key,entry.value)
      if (keys == null || keys.contains(entry.key))
	index.add(edge,entry.key,String.valueOf(entry.value))
    }
    g.stopTransaction(TransactionalGraph.Conclusion.SUCCESS)
    return edge
  } catch (e) { 
    g.stopTransaction(TransactionalGraph.Conclusion.FAILURE)
    return e
  }
}

// Uniqueness
def ensure_unique_for_index(index_name, key, initial) {
  // Neo4jTokens.QUERY_HEADER = "%query%"
  def idx = g.idx(index_name)
  def trys = 1
  def attempt = initial
  while(idx.get(key, Neo4jTokens.QUERY_HEADER + attempt).hasNext())
    attempt = String.sprintf("%s-%d", initial, ++trys)
  return attempt
}

// Indices
def query_exact_index(index_name, key, query_string) {
  // Neo4jTokens.QUERY_HEADER = "%query%"
  return g.idx(index_name).get(key, Neo4jTokens.QUERY_HEADER + query_string)
}

// Fetch relations as well as queried objects
def query_exact_index_with_related(index_name, key, query_string, outRels, inRels) {
  // Neo4jTokens.QUERY_HEADER = "%query%"
  pipe = g.idx(index_name).get(key, Neo4jTokens.QUERY_HEADER + query_string)
  if (!pipe.hasNext() || (outRels.size == 0 && inRels.size == 0))
    return pipe
  return pipe._().copySplit(*(
      [_()] + outRels.collect{_().out(it)} + inRels.collect{_().in(it)}
    )
  ).exhaustMerge() 
}

// Metadata

def get_metadata(key, default_value) {
  neo4j = g.getRawGraph();
  properties = neo4j.getKernelData().properties();
  return properties.getProperty(key, default_value);
}

def set_metadata(key, value) {  
  g.setMaxBufferSize(0)
  g.startTransaction()
  try {
    neo4j = g.getRawGraph();
    properties = neo4j.getKernelData().properties();
    resp = properties.setProperty(key, value);
    g.stopTransaction(TransactionalGraph.Conclusion.SUCCESS)
    return resp
  } catch (e) { 
    g.stopTransaction(TransactionalGraph.Conclusion.FAILURE)
    return e
  }
}

def remove_metadata(key) {
  g.setMaxBufferSize(0)
  g.startTransaction()
  try {
    neo4j = g.getRawGraph();
    properties = neo4j.getKernelData().properties();
    resp = properties.removeProperty(key);
    g.stopTransaction(TransactionalGraph.Conclusion.SUCCESS)
    return resp
  } catch (e) { 
    g.stopTransaction(TransactionalGraph.Conclusion.FAILURE)
    return e
  }
}


// 
// Copyright 2012 James Thornton (http://jamesthornton.com)
// BSD License (see LICENSE for details)
//
// Gremlin scripts in Gremlin-Groovy v1.3
//
// See the Gremlin and Blueprints docs for the full Gremlin/Blueprints API. 
//
// Gremlin Wiki:     https://github.com/tinkerpop/gremlin/wiki
// Gremlin Steps:    https://github.com/tinkerpop/gremlin/wiki/Gremlin-Steps
// Gremlin Methods:  https://github.com/tinkerpop/gremlin/wiki/Gremlin-Methods 
// Blueprints Wiki:  https://github.com/tinkerpop/blueprints/wiki
//
// Client-specific methods are defined at the client level.
// e.g. neo4j/gremlin.groovy and rexster/gremlin.groovy
//

// Graph

def get_vertices() { 
  g.getVertices()
}

def get_edges() {
  g.getEdges()
}

// Vertices

// These edge-label conditionals are a messy hack until Gremin allows null labels. 
// See https://github.com/tinkerpop/gremlin/issues/267

// the || label == "null" is a hack until Rexster fixes its null label bug.
// See https://github.com/tinkerpop/rexster/issues/197

def outE(_id, label) {
  if (label == null)
    g.v(_id).outE()
  else
    g.v(_id).outE(label)
}

def inE(_id, label) {
  if (label == null)
    g.v(_id).inE()
  else
    g.v(_id).inE(label)
}

def bothE(_id, label) { 
  if (label == null)
    g.v(_id).bothE()
  else
    g.v(_id).bothE(label)
}

def outV(_id, label) {
  if (label == null)
    g.v(_id).out()
  else
    g.v(_id).out(label)
}

def inV(_id, label) {
  if (label == null)
    g.v(_id).in()
  else
    g.v(_id).in(label)
}

def bothV(_id, label) { 
  if (label == null)
    g.v(_id).both()
  else
    g.v(_id).both(label)
}

def v(_id) {
  g.v(_id)
}

// Neo4j requires you delete all adjacent edges first. 
// Blueprints' removeVertex() method does that; the Neo4jServer DELETE URI does not.
def delete_vertex(_id) {
  vertex = g.v(_id)
  g.removeVertex(vertex)
}

// Indices

def index_count(index_name, key, value) {
  index = g.idx(index_name);
  return index.count(key,value);
}

def get_or_create_vertex_index(index_name, index_params) {
  def getOrCreateVertexIndex = { 
    index = g.idx(index_name)
    if (index == null) {
      if (index_params == null) {
        index = g.createManualIndex(index_name, Vertex.class)
      } else {
        index = g.createManualIndex(index_name, Vertex.class, index_params)
      }
    }
    return index
  }
  def transaction = { final Closure closure ->
    g.setMaxBufferSize(0);
    g.startTransaction();
    try {
      results = closure();
      g.stopTransaction(TransactionalGraph.Conclusion.SUCCESS);
      return results; 
    } catch (e) {
      g.stopTransaction(TransactionalGraph.Conclusion.FAILURE);
      throw e;
    }
  }
  return transaction(getOrCreateVertexIndex);
}

def get_or_create_edge_index(index_name, index_params) {
  def getOrCreateEdgeIndex = { 
    index = g.idx(index_name)
    if (index == null) {
      if (index_params == null) {
        index = g.createManualIndex(index_name, Edge.class)
      } else {
        index = g.createManualIndex(index_name, Edge.class, index_params)
      }
    }
    return index
  }
  def transaction = { final Closure closure ->
    g.setMaxBufferSize(0);
    g.startTransaction();
    try {
      results = closure();
      g.stopTransaction(TransactionalGraph.Conclusion.SUCCESS);
      return results; 
    } catch (e) {
      g.stopTransaction(TransactionalGraph.Conclusion.FAILURE);
      throw e;
    }
  }
  return transaction(getOrCreateEdgeIndex);
}

// Utils

def warm_cache() {
  for (vertex in g.getVertices()) {
    vertex.getOutEdges()
  }
}

def load_graphml(uri) {
  g.loadGraphML(uri)
}

def save_graphml() {
  g.saveGraphML('bulbs.graphml')
  new File('bulbs.graphml').getText()
}

def clear() {
  g.clear()
}

//
// Gremln user-defined steps
//
// You must execute the step definition script at least once before you 
// use the steps. For production use, this should really be defined server side.
//

// Tree Steps: inTree() and outTree()
// See https://groups.google.com/d/topic/gremlin-users/iCPUifiU_wk/discussion


// Obsoleted with the addition of the the TreePipe in Gremlin 1.6 
// See https://groups.google.com/d/topic/gremlin-users/9s2YWsqK_Ro/discussion

def define_tree_steps() {
  tree = { vertices ->

    def results = []

    vertices.each() {
      results << it
      children = it."$direction"().toList()
      if (children) {
        child_tree = tree(children)
        results << child_tree
      }
    }
    results
  }

  inClosure = {final Object... params -> 
    try { label = params[0] }
    catch(e){ label = null }
    results = []
    direction = "in"
    _().transform{ tree(it) }
  }    

  outClosure = {final Object... params -> 
    try { label = params[0] }
    catch(e){ label = null }
    results = []
    direction = "out"
    _().transform{ tree(it) }
  }   

  Gremlin.defineStep("inTree", [Vertex,Pipe], inClosure) 
  Gremlin.defineStep("outTree", [Vertex,Pipe], outClosure) 
}

