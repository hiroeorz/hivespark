var HS = HS || {};

/**
 * チーム情報の表示
 *
 * @namespace HS
 * @class TeamEditViewController
*/

HS.TeamEditViewController = function () { 'use strict'; this.init(); };

HS.TeamEditViewController.prototype = (function () {
    'use strict';

    /* private */

    var get_team_info = function (team_id) {
	$.getJSON("/team/info?team_id=" + team_id,
		  function (json) {
		      var team = json.team;
		      $("input#name").attr("value", team.name);
		      $("textarea#description").attr("value", team.description);
		      $("#status-" + team.status).attr("selected", "selected");
		      $("textarea#status_description").attr("value", 
					              team.status_description);
		      $("#team-show").attr("href", 
					   "/team/show?team_id=" + team_id);
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
	viewDidLoad: function () {
	    var self = this;
	    var params = new HS.Util().qs_vals();
	    
	    $("input#team_id").attr("value", params.team_id);
	    get_team_info(params.team_id);
	}

    };
}());