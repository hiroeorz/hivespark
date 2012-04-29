var HS = HS || {}
HS.Util = function() {this.init();}

HS.Util.prototype = {
    init: function() {
    },

    qs_vals:function() {
	var arg = new Object;
	var pair=location.search.substring(1).split('&');

	for(i=0; pair[i]; i++) {
	    var kv = pair[i].split('=');
	    arg[kv[0]]=kv[1];
	}

	return arg;
    }
}