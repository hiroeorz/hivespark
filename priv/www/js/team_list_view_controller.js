var HS = HS || {};

/**
 * チーム情報のリスト表示
 *
 * @namespace HS
 * @class TeamListViewController
*/

HS.TeamListViewController = function () { 'use strict'; this.init(); };

HS.TeamListViewController.prototype = (function () {

    /* private */
	
    var get_usr_info = function() {
	$.getJSON("/usr/show_myself",
		  function(json) {
		      $("div#profile img").attr("src", json.usr.icon_url);
		      $("span#my-project-count").text(json.usr.team_id_count);
		  });
    };

    var createTeamView =  function(team, selector) {
	var team_view = document.createElement("li");

	var team_img = document.createElement("img");
	team_img.src = team.icon_url;
	team_img.alt = team.name;

	var team_img_a = document.createElement("a");
	team_img_a.href = "/team/show?team_id=" + team.id;
	team_img_a.appendChild(team_img);	
	team_view.appendChild(team_img_a);
	
	var members_ul = document.createElement("ul");
	members_ul.className = "members";
	team_view.appendChild(members_ul);
	
	for (var i = 0;  i < team.members.length; i++) {
	    if (i > 4) { break; }
	    var member = team.members[i];
	    var member_li = document.createElement("li");
	    var member_img = document.createElement("img");
	    member_img.src = member.icon_url;
	    member_li.appendChild(member_img);
	    members_ul.appendChild(member_li);
	}

	var project_name_h3 = document.createElement("h3");
	var project_name = document.createTextNode(team.name);
	project_name_h3.appendChild(project_name)
	team_view.appendChild(project_name_h3);

	var name_a = document.createElement("a");
	name_a.href = "/team/show?team_id=" + team.id;
	name_a.appendChild(project_name_h3);	
	team_view.appendChild(name_a);
	
	var description_p = document.createElement("p");
	var description = document.createTextNode(team.description);
	description_p.appendChild(description);
	team_view.appendChild(description_p);
	
	selector.append(team_view);
    };

    /* public */

    return {
	/**
	 * 初期化
	 *
	 * @method init
	 */
	init: function() {
	},
	
	/**
	 * 画面表示直後に呼ばれる
	 *
	 * @method view_did_load
	 */
	view_did_load: function() {
	    var self = this;
	    get_usr_info();
	    self.reload_all_team_views();
	},
	
	/**
	 * チームリストを更新する
	 *
	 * @method reloadAllTeamViews
	 */
	reload_all_team_views: function() {
	    var self = this;
	    $("ul#burning-teams").empty();
	    $("ul#stop-teams").empty();
	    
	    $.getJSON("/team/statuses_list",
		      function(json) {
			  var stop_teams = json.stop_teams;
			  var burning_teams = json.burning_teams;
			  var testing_teams = json.testing_teams;

			  for (var i = 0; i < burning_teams.length; i++) {
			      createTeamView(burning_teams[i], 
					     $("ul#burning-teams"));
			  }
			  for (var i = 0; i < stop_teams.length; i++) {
			      createTeamView(stop_teams[i], 
					     $("ul#stop-teams"));
			  }
		      });
	}	
    };
}());