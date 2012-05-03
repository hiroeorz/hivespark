var HS = HS || {};

/**
 * チーム情報の表示
 *
 * @namespace HS
 * @class TeamViewController
*/

HS.UsrEditViewController = function () { 'use strict'; this.init(); };

HS.UsrEditViewController.prototype = (function () {
    'use strict';

    /* private */

    /**
     * ユーザーの情報を取得してHTML上に配置します
     *
     * @method get_user_info
     */
    var get_user_info = function () {
	$.getJSON("/usr/show_myself",
		  function (json) {
		      $("div#profile img").attr("src", json.usr.icon_url);
		      $("#name").val(json.usr.name);
		      $("#longname").val(json.usr.longname);
		      $("#email").val(json.usr.email);
		      $("#description").val(json.usr.description);
		  });
    };

    /**
     * 各オブジェクトのイベントリスナを登録します
     *
     * @method set_default_event_lister
     */
    var set_default_event_lister = function(params) {
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
	    set_default_event_lister(params);
	    get_user_info();
	}

    };
}());

