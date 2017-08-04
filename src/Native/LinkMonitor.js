var _cacay$elm_router$Native_LinkMonitor = function() {
    var fakeNode = {
        addEventListener: function() {},
        removeEventListener: function() {}
    };

    function onClick(decoder, toTask) {
        var node = typeof document !== 'undefined' ? document.body : fakeNode;

        return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {

            function performTask(event)
            {
                var result = A2(_elm_lang$core$Json_Decode$decodeValue, decoder, event);
                if (result.ctor === 'Ok')
                {
                    event.preventDefault();
                    _elm_lang$core$Native_Scheduler.rawSpawn(toTask(result._0));
                }
            }

            node.addEventListener("click", performTask);

            return function()
            {
                node.removeEventListener("click", performTask);
            };
        });
    }


    // SCROLLING

    var rAF = typeof requestAnimationFrame !== 'undefined'
        ? requestAnimationFrame
        : function(callback) { callback(); };

    function withNode(id, doStuff)
    {
        return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
        {
            rAF(function()
            {
                var node = document.getElementById(id);
                if (node === null)
                {
                    callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'NotFound', _0: id }));
                    return;
                }
                callback(_elm_lang$core$Native_Scheduler.succeed(doStuff(node)));
            });
        });
    }

    function scrollTo(id)
    {
        return withNode(id, function(node) {
            // TODO: research better methods
            node.scrollIntoView(true);
            //window.scrollTo(0, node.offset().top);
            return _elm_lang$core$Native_Utils.Tuple0;
        });
    }


    return {
        onClick: F2(onClick),
        scrollTo: scrollTo,
    };

}();

