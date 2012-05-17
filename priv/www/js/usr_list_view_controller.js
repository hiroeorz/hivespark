var HS = HS || {};

/**
 * チーム情報の表示
 *
 * @namespace HS
 * @class TeamViewController
*/

HS.UsrListViewController = function () { 'use strict'; this.init(); };

HS.UsrListViewController.prototype = (function () {
    'use strict';

    /* private */

    /**
     * サーバからユーザ情報を取得してリスト表示します
     *
     * @method get_usr_list
     */
    var get_usr_list = function() {
	$("ul#timeline").empty();

	$.getJSON("/usr/all", {}, 
		  function(usrs) {
		      for (var i = 0; i < usrs.length; i++) {
			  var usr_li = create_list_of_usr_view(usrs[i]);
			  $("ul#timeline").append(usr_li);
		      }
		  });
    };

    /**
     * ユーザ一人分のビューを作って返します
     *
     * @method create_list_of_usr_view
     */
    var create_list_of_usr_view = function(usr) {
	var li = document.createElement("li");

	var img = document.createElement("img")
	img.src = usr.icon_url;
	img.title = usr.name;
	img.alt = usr.name;

	var name_b = document.createElement("b");
	var name_text = document.createTextNode(usr.longname);
	name_b.appendChild(name_text);
	
	var description_p = document.createElement("p");
	var description_text = document.createTextNode(usr.description);
	description_p.appendChild(description_text);

	li.appendChild(img);
	li.appendChild(name_b);
	li.appendChild(description_p);
	return li;
    }

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
	    get_usr_list();
	}

    };
}());

