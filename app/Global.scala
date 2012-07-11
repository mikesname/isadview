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

    relationships.Describes.initialize()
    relationships.LocatesInTime.initialize()
    relationships.LocatesInSpace.initialize()
    relationships.HeldBy.initialize()
    relationships.HasCollection.initialize()
    relationships.Contains.initialize()
    relationships.AddressOf.initialize()
    relationships.MentionedIn.initialize()
    relationships.CreatedBy.initialize()
  }
}


