/**
 * @module events4js
 */
var EventProducer = (function () {

    /**
     * Abstract class for managing events
     * @class EventProducer
     * @constructor
     **/
    EventProducer = function () {
        this.initialize();
    }
    var p = EventProducer.prototype;



    // constructor:
    /** 
     * @method initialize
     * @description Initialization method.
     * @protected
     **/
    p.initialize = function (config) {
        this._e4js = {};
        this._e4js.events = {};
        this._e4js.allevents = [];
        this._e4js.onces = {};
        this._e4js.eventsfollow = [];
        if (config != undefined) {
            if (config.autoLaunch != undefined) {
                for (var i in config.autoLaunch) {
                    for (var ii in config.autoLaunch[i]) {
                        this.addEventListener(config.autoLaunch[i][ii], this._repeatEvent_fnct_producer(i));
                    }
                }
            }
            if (config.waitFor != undefined) {
                this._e4js.waitFor = {};
                this._e4js.waitFor.t = {};
                this._e4js.waitFor.values = {};
                this.addAllEventsListener(this._computeEvents_fnct_producer());
                for (var i in config.waitFor) {
                    this._e4js.waitFor.values[i] = {};
                    var cwfi = config.waitFor[i];
                    for (var ii in cwfi) {
                        this._e4js.waitFor.values[i][cwfi[ii]] = false;
                        if (this._e4js.waitFor.t[cwfi[ii]] == undefined) {
                            this._e4js.waitFor.t[cwfi[ii]] = [];
                        }
                        this._e4js.waitFor.t[cwfi[ii]].push(i);
                    }
                }
            }
        }
    };
    // public methods:
    /**
     * @description Add an event listener
     * @param eventName {string} the name of the event listened
     * @param callback {function} the function trigged
     * @method addEventListener
     **/
    p.addEventListener = function (eventName, callback) {
        if (this._e4js.events[eventName] == undefined) {
            this._e4js.events[eventName] = [callback];
        }
        else {
            this._e4js.events[eventName].push(callback);
        }
    };
    /**
     * @description Add an once listener
     * @param {string} eventName the name of the event listened
     * @param {function} callback the function trigged
     * @method addOnceListener
     **/
    p.addOnceListener = function (eventName, callback) {
        if (this._e4js.onces[eventName] == undefined) {
            this._e4js.onces[eventName] = [callback];
        }
        else {
            this._e4js.onces[eventName].push(callback);
        }
    };
    /**
     * @description Add all events listener
     * @param {function} callback the function trigged
     * @method addAllEventsListener
     **/
    p.addAllEventsListener = function (callback) {
        this._e4js.allevents.push(callback);
    };
    /**
     * @description Remove a listener
     * @private
     * @param {string} eventName the name of the event listened
     * @param {function} callback the function trigged
     * @param {array} ar the array of function where we want to remove
     * @method _removeListener
     **/
    p._removeListener = function (eventName, callback, ar) {
        if (ar[eventName] != undefined) {
            for (var ee in ar[eventName]) {
                if (ar[eventName][ee] === callback) {
                    ar[eventName].splice(ee, 1);
                    break;
                };
            }
        }
    };
    p._addEventFollow = function (eventName) {
        this._e4js.eventsfollow.push(eventName);
    };
    p._repeatEvent_fnct_producer = function (eventName) {
        var en = eventName;
        var t = this;
        var fn = this._addEventFollow;
        return function () {
            fn.apply(t, [en])
        };
    };


    p._computeEvents = function (en, e) {
        if (this._e4js.waitFor.t[en] != undefined) {
            for (var i in this._e4js.waitFor.t[en]) {
                var master_en = this._e4js.waitFor.t[en][i];
                this._e4js.waitFor.values[master_en][en] = true;
            }
            var wfv = this._e4js.waitFor.values;
            for (var i in wfv) {
                var is_ready = true;
                for (var ii in wfv[i]) {
                    is_ready = is_ready && wfv[i][ii];
                }
                if (is_ready) {
                    for (var ii in wfv[i]) {
                        wfv[i][ii] = false;
                    }
                    this._e4js.eventsfollow.push(i)
                }
            }
        }
    };

    p._computeEvents_fnct_producer = function () {
        var t = this;
        var fn = this._computeEvents;
        return function (en, e) {
            fn.apply(t, [en, e])
        };
    };
    /**
     * @description Remove an event listener
     * @param {string} eventName the name of the event listened
     * @param {function} callback the function trigged
     * @method removeEventListener
     **/
    p.removeEventListener = function (eventName, callback) {
        this._removeListener(eventName, callback, this._e4js.events);
    }
    /**
     * @description Remove an once listener
     * @param {string} eventName the name of the once listened
     * @param {function} callback the function trigged
     * @method removeOnceListener
     **/
    p.removeOnceListener = function (eventName, callback) {
        this._removeListener(eventName, callback, this._e4js.onces);
    }
    /**
     * @description Remove all event listeners
     * @param {string} eventName the name of the event listened
     * @method removeAllEventListeners
     **/
    p.removeAllEventListeners = function (eventName) {
        this._e4js.events[eventName] = [];
    };
    /**
     * @description Remove all once listeners
     * @param {string} eventName the name of the once listened
     * @method removeAllOnceListeners
     **/
    p.removeAllOnceListeners = function (eventName) {
        this._e4js.onces[eventName] = [];
    };
    /**
     * @description Remove all events listeners - for all events
     * @method removeAllEventsListeners
     **/
    p.removeAllEventsListeners = function () {
        this._e4js.events = {};
    };
    /**
     * @description Fire an Event
     * @param {string} eventName the name of the event fired
     * @param {object} params optional : information about the event, forward to listeners
     * @method fireEvent
     **/
    p.fireEvent = function (eventName, params) {
        this._e4js.eventsfollow = [];
        if (this._e4js.onces[eventName] !== undefined) {
            for (var ee in this._e4js.onces[eventName]) {
               this._e4js.onces[eventName][ee](params);
               this._e4js.onces[eventName].splice(ee, 1);
            }
        }
        if (this._e4js.events[eventName] !== undefined) {
            for (var ee in this._e4js.events[eventName]) {
               this._e4js.events[eventName][ee](params);
            }
        }
        for (var ee in this._e4js.allevents) {
            this._e4js.allevents[ee](eventName, params);
        }
        for (var ee in this._e4js.eventsfollow) {
            this.fireEvent(this._e4js.eventsfollow[ee], params);
        }
        this._e4js.eventsfollow = [];
    };

    return EventProducer;

})();
