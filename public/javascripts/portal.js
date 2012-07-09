jQuery(function($) {

    // Openid
    $("form.openid").openid();

    // Chosen selects...
    $(".chzn-select").chosen();

    // Date picker...
    $("input[type='date']").datepicker({
        format: "yyyy-mm-dd"
    })

    $(".more-facets").each(function(i, elem) {
        $(elem).modal({backdrop:true, 
            shown: function(e) { alert("show"); },
        });
    });

    $("a.show-save-item-list").click(function(event) {
        $(".save-item-list").hide();
        event.preventDefault();
        $(document).bind("click.close-save-list", function(event) {
            $(".save-item-list").hide();
            $(this).unbind("click.close-save-list");
        });
        var $div = $(this).next("div")
        $div.click(function(event) {
            event.stopPropagation();
        }).css({
            left: $(this).position().left - $div.width()        
        }).fadeIn(100);
        event.stopPropagation();
    });

    function handleSaveLinkClick(elem, event) {
        event.preventDefault();
        console.log("Got click");
        var $elem = $(elem);
        $.post($elem.attr("href"), function(data) {
            if (data && data.ok) {
                $elem.closest(".save-item-list")
                    .hide(200)
                    .prev("a").find("i").attr("class", "icon-star");
            }
        }, "json");
    }


    //$("body").on("click", "a.save-item", function(event) {
    $("a.save-item").click(function(event) {
        handleSaveLinkClick(this, event);
    });

    $(".stash-new-collection-form").submit(function(event) {
        event.preventDefault();
        // submit the form with Ajax - we should recieve back some details
        // of the newly created collection, which we can use to insert into
        // the other lists...
        var $elem = $(this);
        $.post($(this).attr("action"), $(this).formSerialize(), function(data) {
            if (data && data.ok) {
                $(".bookmark-controls").each(function(i, item) {
                    var itemid = $(item).data("item");
                    var url = jsRoutes.controllers.VirtualCollections.saveItem(itemid, data.id).url;
                    // Add a link for adding subsequent items to this collection
                    var $link = $("<a></a>").attr("href", url).addClass("save-item").text(data.name);

                    // BAH! Should be able to do this with live events, but because
                    // the element is initially hidden it doesn't seem to work...
                    $link.click(function(event) {
                        handleSaveLinkClick(this, event);
                    });
                    $(item).find("ul").append($("<li></li>").append($link));
                    $elem.closest(".save-item-list")
                        .hide(200)
                        .prev("a").find("i").attr("class", "icon-star");
                });
            }
        }, "json");
    });

    //add Ajax behaviour on pagination links
    $(document).on("click", ".ajax, #facet-popup > .pagination ul li a", function(event) {
        // MASSIVE HACK - extract the facet class from the fclass data attribute
        // that is written into the modal-body div and alter the various
        // links to point the the particular facet list page.  This is deeply
        // offensive to all that is good and pure...
        var klass = $("#facet-popup").data("fclass");
        if (klass && this.href) {
            event.preventDefault();
            $("#modal-popup").load(this.href.replace(/search\/?\?/, "search/" + klass + "/?"));
        }
    });

    $(".facet-header").click(function(event) {
        event.preventDefault();
        $(this).parent().next("dl").toggle(200);
    });

    // OpenID buttons
    $("a.openid-button").click(function(event) {
        event.preventDefault();
        $("input[name='openid_url']").val(this.href).closest("form").submit();
    });

    // Make sure no more than two revision diff checkboxes are
    // checked at the same time
    $("input[type='submit']", $(".object-version-history")).prop("disabled", true);
    $("input[type='checkbox']", $(".object-version-history")).click(function(event) {        
        var checked = $("input[type='checkbox']:checked", $(".object-version-history"));
        if (checked.length > 2) {
            checked.not(this).prop("checked", false);
        }
        $("input[type='submit']", $(".object-version-history")).prop(
                "disabled", checked.length != 2);
    });
});



//jQuery(function($) {
//
//    // Handle slide-out suggestions form
//    //
//    if (window.TAB_PATH) {
//        var $slider = $('.slide-out-div').tabSlideOut({
//            tabHandle: '.handle',                              
//            pathToTabImage: window.TAB_PATH, 
//            imageHeight: '75px',                               
//            imageWidth: '24px',                               
//            tabLocation: 'left',                               
//            speed: 300,                                        
//            action: 'click',                                   
//            topPos: '50px',                                   
//            fixedPosition: true,
//            onSlideOut: function() {
//            },
//            onSlideIn: function() {
//            },
//        });
//
//        // handle suggestion form submission... this is a bit
//        // gross and fragile.
//        var $form = $("#suggestion-form"),
//            $submit = $("button[type='submit']", $form),
//            $thanks = $(".alert-success", $form),
//            $name = $("#id_suggestion-name", $form),
//            $text = $("#id_suggestion-text", $form),
//            $email = $("#id_suggestion-email", $form),
//            $emailregexp = /^[a-zA-Z0-9._-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,4}$/;
//        $(".slide-out-div").show();
//        $name.add($text).add($email).keyup(function(event) {
//            var nameval = $.trim($name.val());
//            var textval = $.trim($text.val());
//            var emailval = $.trim($email.val());
//            // email is not reqired, so only check it if filled in
//            var emailvalid = (emailval === "" || emailval !== "" && emailval.match($emailregexp));
//            var ok = nameval !== "" && textval !== "" && emailvalid;
//            $submit.prop("disabled", !ok);
//        });
//
//        $(".modal-close", $form).click(function() {
//            $(".slide-out-div > .handle").click(); 
//        });
//
//        $submit.prop("disabled", true).click(function(event) {
//            event.preventDefault();
//            $submit.prop("disabled", true);
//            var $formele = $(this).closest("form");
//            $.post($formele.attr("action"), $formele.serialize(), function(data, textStatus) {
//                // FIXME: This is rubbish.
//                $thanks.width($thanks.parent().width() - ($thanks.outerWidth(true) - $thanks.width()));
//                $thanks.slideDown(500, function() {
//                    setTimeout(function() {
//                        $(".slide-out-div > .handle").click();
//                        $text.val("");
//                        $thanks.slideUp(500); 
//                    }, 1000);
//                });
//            });
//        });
//    }
//});
