jQuery(function($) {
  window.Timeliner = new Timeliner({
    el: $('.recline-app')
  });
  //window.Timeliner._onLoadURL();
});

var Timeliner = Backbone.View.extend({
  events: {
    'submit': 'submitForm'
  },

  initialize: function() {
    this.el = $(this.el);
    this.timeline = null;
    this.explorerDiv = $('.data-views');
    _.bindAll(this, 'viewExplorer', 'viewHome');

    this.router = new Backbone.Router();
    this.router.route('', 'home', this.viewHome);
    this.router.route(/explorer/, 'explorer', this.viewExplorer);
    Backbone.history.start();
  },

  viewHome: function() {
    this.switchView('home');
  },

  viewExplorer: function() {
    this.router.navigate('explorer');
    this.switchView('explorer');
  },

  switchView: function(path) {
    console.log("Hiding page: " + path);
    $('.backbone-page').hide(); 
    var cssClass = path.replace('/', '-');
    console.log("CSS class: " + cssClass);
    $('.page-' + cssClass).show();
  },

  // make Explorer creation / initialization in a function so we can call it
  // again and again
  createExplorer: function(dataset) {
    var self = this;
    // remove existing data explorer view
    var reload = false;
    if (this.timeline) {
      this.timeline.remove();
      reload = true;
    }
    this.timeline = null;
    var $el = $('.data-views .timeline');
    // explicitly set width as otherwise Timeline does extends a bit too far (seems to use window width rather than width of actual div)
    // $el.width((this.el.width() - 45)/2.0);
    this.timeline = new recline.View.Timeline({
      model: dataset,
      el: $el
    });
    this.timeline.render();
    this.timeline.convertRecord = function(record, fields) {
      try {
        var out = this._convertRecord(record, fields);
      } catch (e) {
        out = null;
      }
      if (!out) {
        alert('Failed to extract date from: ' + JSON.stringify(record.toJSON()));
        return null;
      }
      if (record.get('image')) {
        out.asset = {
          media: record.get('image')
        };
      }
      out.text = record.get('description');
      if (record.get('source')) {
        var s = record.get('source');
        if (record.get('sourceurl')) {
          s = '<a href="' + record.get('sourceurl') + '">' + s + '</a>';
        }
        out.text += '<p class="source">Source: ' + s + '</p>';
      }
      // hacky but it will work ...
      // do not want time part of the dates
      out.startDate = String(out.startDate.getFullYear()) + ',' + String(out.startDate.getMonth()+1) + ',' + String(out.startDate.getDate());
      return out;
    }

    this.map = new recline.View.Map({
      model: dataset
    });
    this.explorerDiv.append(this.map.el);
    this.map.render();

    // show the view
    this.viewExplorer();
    // load the data
    dataset.fetch();
  },

  submitForm: function(e) {
    e.preventDefault();
    var self = this;
    $.ajax({
        url: window.location.path,
        success: function(data) {
            console.log("Got data: " + data)
            dataset = new recline.Model.Dataset({
                backend: "memory",
                records: data
            });
            self.createExplorer(dataset);
        }
    });
  }
});

