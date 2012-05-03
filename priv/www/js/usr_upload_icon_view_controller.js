var HS = HS || {};

/**
 * チーム情報の表示
 *
 * @namespace HS
 * @class UsrUploadIconViewController
*/

HS.UsrUploadIconViewController = function () { 'use strict'; this.init(); };

HS.UsrUploadIconViewController.prototype = (function () {
    'use strict';

    /* private */

    var get_usr_info = function (usr_id) {
	$.getJSON("/usr/show_myself",
		  function (json) {
		      var usr = json.usr;
		      $("div#profile img").attr("src", usr.icon_url);
		      $("#usr-image").attr("src", usr.icon_url);
		      $("#usr-image").attr("alt", usr.name);
		  });
    };

    /* public */
    return {
	/**
	 * 初期化
	 *
	 * @method init
	 */
	init: function () {
	},
	
	/**
	 * 画面表示直後に呼ばれる
	 *
	 * @method view_did_load
	 */
	view_did_load: function () {
	    var self = this;
	    var params = new HS.Util().qs_vals();
	    
	    get_usr_info(params.usr_id);
	}

    };
}());