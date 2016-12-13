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

    return {
        onClick: F2(onClick),
    };

}();

