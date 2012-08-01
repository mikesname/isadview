package controllers

import scala.reflect.Manifest

import play.api._
import play.api.mvc._
import play.api.libs.ws.{WS,Response}
import play.api.libs.concurrent.Promise
import play.api.libs.concurrent.execution.defaultContext

import net.liftweb.json

import com.codahale.jerkson.Json._

import models.{Repository,Contact,Collection,FuzzyDate,Authority}
import forms.CollectionForm



object Collections extends AuthController with ControllerHelpers {
  def detail(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Async {
      Collection.fetchBySlug(slug).map { collection =>
        Ok(views.html.collection.detail(collection, collection.description))
      }
    }
  }

  def create(repo: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    val action = routes.Collections.createPost(repo)
    Ok(views.html.collection.form(f=CollectionForm.form, action=action))
  }

  def createPost(repo: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    // transform input for multiselects
    val formData = transformMultiSelects(request.body.asFormUrlEncoded, List(
      "control.languagesOfDescription",
      "control.scriptsOfDescription"
    ))

    CollectionForm.form.bindFromRequest(formData).fold(
      errorForm => {
        BadRequest(
          views.html.collection.form(f=errorForm, action=routes.Collections.createPost(repo)))
      },
      data => {
        Async {
          Repository.fetchBySlug(repo).flatMap { repository =>
            Collection.create(new Collection(description=data)).flatMap { created =>
              Repository.createRelationship(created, repository, "heldBy").map { edge =>
                Redirect(routes.Collections.detail(slug=created.slug.get))
              }
            }
          }
        }
      }
    )
  }

  def edit(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Async {
      Collection.fetchBySlug(slug).map { collection =>
        val form = CollectionForm.form.fill(collection.description)
        val action = routes.Collections.save(slug)
        Ok(views.html.collection.form(f=form, action=action, c=Some(collection)))
      }
    }
  }

  def save(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    // transform input for multiselects
    val formData = transformMultiSelects(request.body.asFormUrlEncoded, List(
      "conditions.languages",
      "conditions.scripts",
      "control.languagesOfDescription",
      "control.scriptsOfDescription"
    ))

    Async {
      Collection.fetchBySlug(slug).map { collection =>
        CollectionForm.form.bindFromRequest(formData).fold(
          errorForm => {
            BadRequest(
            views.html.collection.form(f=errorForm,
            action=routes.Collections.save(slug), c=Some(collection)))
          },
          data => {
            Async {
              Collection.persist(collection.id, collection.copy(description=data)).map { updated =>
                Redirect(routes.Collections.detail(slug=updated.slug.get))
              }
            }
          }
        )
      }
    }
  }
  
  def confirmDelete(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Async {
      Collection.fetchBySlug(slug).map { collection =>
        val action = routes.Collections.delete(slug)
        Ok(views.html.basedelete(c=collection, action=action))
      }
    }
  }

  def delete(slug: String) = optionalUserProfileAction { implicit maybeUser => implicit request =>
    Async {
      Collection.fetchBySlug(slug).map { collection =>
        Collection.delete(collection.id, collection)
        Redirect(routes.Search.list("collection"))
      }
    }
  }

  def importForm(repo: String) = authorizedAction(models.sql.Administrator) { user => implicit request =>
    Ok(views.html.importForm(user, routes.Collections.importPost(repo)))
  }


  import scala.io.Source
  import scala.xml.XML._
  import scala.xml.pull._
  import scala.xml._

  // Pinched from:
  // http://stackoverflow.com/questions/8525675/how-to-get-a-streaming-iteratornode-from-a-large-xml-document
  def processSource[T](input: Source)(f: scala.xml.NodeSeq => T) {
    new scala.xml.parsing.ConstructingParser(input, false) {
      var depth = 0 // track depth
      nextch // initialize per documentation
      document // trigger parsing by requesting document

      override def elemStart(pos: Int, pre: String, label: String,
          attrs: MetaData, scope: NamespaceBinding) {
        super.elemStart(pos, pre, label, attrs, scope)
        depth += 1
      }
      override def elemEnd(pos: Int, pre: String, label: String) {
        depth -= 1
        super.elemEnd(pos, pre, label)
      }
      override def elem(pos: Int, pre: String, label: String, attrs: MetaData,
          pscope: NamespaceBinding, nodes: NodeSeq): NodeSeq = {
        val node = super.elem(pos, pre, label, attrs, pscope, nodes)
        depth match {
          case 1 => <dummy/> // dummy final roll up
          case 2 => f(node); NodeSeq.Empty // process and discard employee nodes
          case _ => node // roll up other nodes
        }
      }
    }
  }

  // Testing an asyncronous XML parser...
  def aaltoTest() = {
    import com.fasterxml.aalto.{AsyncXMLStreamReader,AsyncInputFeeder}
    import com.fasterxml.aalto.stax.InputFactoryImpl
    import javax.xml.stream.XMLStreamConstants._

    val buf: Array[Byte] = "<html>Very <b>simple</b> input document!</html>".getBytes
    val reader: AsyncXMLStreamReader = new InputFactoryImpl().createAsyncXMLStreamReader
    val feeder: AsyncInputFeeder = reader.getInputFeeder
    var inputPtr = 0 // as we feed byte at a time
    var etype = 0

    do {
      // May need to feed multiple "segments"
      etype = reader.next()
      while (etype == AsyncXMLStreamReader.EVENT_INCOMPLETE) {
        feeder.feedInput(buf, inputPtr, 1)
        inputPtr += 1
        if (inputPtr >= buf.length) { // to indicate end-of-content (important for error handling)
          feeder.endOfInput()
        }
        etype = reader.next()
      }
      // and once we have full event, we just dump out event type (for now)
      println("Got event of type: " + etype)
      if (List(START_ELEMENT, END_ELEMENT, ENTITY_REFERENCE).contains(etype))
        println(reader.getLocalName)
      // could also just copy event as is, using Stax, or do any other normal non-blocking handling:
      // xmlStreamWriter.copyEventFromReader(asyncReader, false);
    } while (etype != END_DOCUMENT)
    reader.close()
  }

  import play.api.libs.iteratee.{Iteratee,Enumerator}

  def uploadTest = Action(parse.raw) {  request =>
    //val processor = processSource(Source.fromFile(request.body.file)) _
    //val channel = Enumerator.pushee[NodeSeq] ( onStart = pushee =>
    //  processor(node => pushee.push(node)) 
    //)
    //SimpleResult(
    //  header=ResponseHeader(200),
    //  body=channel
    //)

    //val enumerator = Enumerator.fromStream(request.body)
    ////val iter = Iteratee.foreach[Array[Byte]](s => s)
    ////val iter2 = enumerator(iter)

    //SimpleResult(
    //  header=ResponseHeader(200),
    //  body=enumerator
    //)
    Ok("done")
  }

  def xmlToGeoff(slug: String, in: String, out: String) = optionalUserAction { implicit maybeUser => implicit request =>
    
    Async {
      Repository.fetchBySlug(slug).map { repo =>
    
        import scalax.io._
        val output = Resource.fromFile(out)
        output.truncate(0)             
        processSource(Source.fromFile(in)) { doc =>
          val importer = doc.head.label match {
            case "doc" => importers.USHMM.docToGeoff _
            case "ead" => importers.EAD.docToGeoff _
            case s => throw sys.error("No importer for type: '%s'".format(s))
          }
          output.writeStrings(importer("repo%d".format(repo.id), doc), separator="\n")(Codec.UTF8)
        }
        Ok("done")
      }
    }
  }

  def importTest(slug: String) = optionalUserAction { implicit maybeUser => implicit request =>

    var file = request.queryString.getOrElse("f", Seq()).headOption.getOrElse(
          throw sys.error("No Geoff file supplied."))
    val size = 100000
    val timeout = 100000L

    import scala.io.Source
    // Let's crash the JVM...
    val lines = Source.fromFile(file).getLines
    val init = Map[String,Map[String,String]]()
    Async {
      models.Repository.fetchBySlug(slug).map { repository =>
        val out = lines.grouped(size).map(_.toList).foldLeft(init) { case(params, lineList) =>
          models.Repository.importGeoff(repository, lineList, params).value.get match {
            case Left(throwable) => throw throwable
            case Right(m) => m
          }
        }
        Ok(generate(out))
      }
    }
  }

  def importPost(repo: String) = optionalUserAction(parse.temporaryFile) { implicit maybeUser => implicit request =>
    import play.api.libs.iteratee.Enumerator

    def repoident(repoid: Long) = "repo%d".format(repoid)

    Async {
      Repository.fetchBySlug(repo).map { repository =>
        processSource(Source.fromFile(request.body.file)) { elem =>
          println(importers.USHMM.docToGeoff(repoident(repository.id), elem))
        }
        Ok("done")
      }
    }
  }

  def updateIndex = optionalUserAction { implicit maybeUser => implicit request =>
    import neo4j.query.Query
    import solr.SolrUpdater

    // Holy moly does this get confusing...
    Async {
      // First, take the initial async list of objects and get their
      // full representations, including relations
      val clist = Collection.query.get().map { list =>
        list.map(c => Collection.fetchBySlug(c.slug.get))
      }
      clist.flatMap { cp =>
        // Now take the List of Promises and convert them into
        // a Promise[List[models.Collection]] using the sequence
        // function.
        Promise.sequence(cp).flatMap { items =>
          SolrUpdater.updateSolrModels(items).map { alldone =>
            Ok("%s".format(alldone.map(r => "%s\n".format(r.body))))  
          }
        }
      }
    }
  }
}
