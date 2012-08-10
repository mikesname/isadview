import play.api._

import models._

object Global extends GlobalSettings {
  
  override def onStart(app: Application) {

    // Model initialization creates an index for
    // each of the models if not already there...
    Collection.initialize()
    Repository.initialize()
    Authority.initialize()
    Contact.initialize()
    FuzzyDate.initialize()
    UserProfile.initialize()
    VirtualCollection.initialize()
    Keyword.initialize()
    Place.initialize()

    Keyword.Describes.initialize()
    FuzzyDate.LocatesInTime.initialize()
    Place.LocatesInSpace.initialize()
    Repository.Holds.initialize()
    Repository.HasAddress.initialize()
    UserProfile.HasVirtualCollection.initialize()
    VirtualCollection.Contains.initialize()
    Authority.MentionedIn.initialize()
    Authority.Created.initialize()

    // Wire up callback to update Solr models
    Collection.addListeners(Collection.Callbacks.create, Collection.Callbacks.update) { item =>
      solr.SolrUpdater.updateSolrModel(item.asInstanceOf[solr.SolrModel])  
    }
    Authority.addListeners(Authority.Callbacks.create, Authority.Callbacks.update) { item =>
      solr.SolrUpdater.updateSolrModel(item.asInstanceOf[solr.SolrModel])  
    }
    Repository.addListeners(Repository.Callbacks.create, Repository.Callbacks.update) { item =>
      solr.SolrUpdater.updateSolrModel(item.asInstanceOf[solr.SolrModel])  
    }
    Collection.addListeners(Collection.Callbacks.delete) { item =>
      solr.SolrDeleter.deleteSolrModel(item.asInstanceOf[solr.SolrModel])  
    }
    Authority.addListeners(Authority.Callbacks.delete) { item =>
      solr.SolrDeleter.deleteSolrModel(item.asInstanceOf[solr.SolrModel])  
    }
    Repository.addListeners(Repository.Callbacks.delete) { item =>
      solr.SolrDeleter.deleteSolrModel(item.asInstanceOf[solr.SolrModel])  
    }
  }
}


