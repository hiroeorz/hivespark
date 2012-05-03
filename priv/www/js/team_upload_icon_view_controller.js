var HS = HS || {};

/**
 * チーム情報の表示
 *
 * @namespace HS
 * @class TeamUploadIconViewController
*/

HS.TeamUploadIconViewController = function () { 'use strict'; this.init(); };

HS.TeamUploadIconViewController.prototype = (function () {
    'use strict';

    /* private */

    var get_team_info = function (team_id) {
	$.getJSON("/team/info?team_id=" + team_id,
		  function (json) {
		      var team = json.team;
		      $("#team-image").attr("src", team.icon_url);
		      $("#team-image").attr("alt", team.name);
		  });
    };

    var get_usr_info = function () {
	$.getJSON("/usr/show_myself",
		  function (json) {
		      $("div#profile img").attr("src", json.usr.icon_url);
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
	 * @method viewDidLoad
	 */
	view_did_load: function () {
	    var self = this;
	    var params = new HS.Util().qs_vals();
	    
	    $("input#team_id").attr("value", params.team_id);
	    $("a#team-show").attr("href", 
				  "/team/show?team_id=" + params.team_id);
	    get_team_info(params.team_id);
	    get_usr_info();
	}

    };
}());