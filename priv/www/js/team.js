function Team() {}
var Team = function() {this.init();}

Team.prototype = {

    init: function() {
	this.server_uri = "ws://localhost:8080/websocket";
	this.socket = null;
    },

    create: function() {
	var name = $("#name").val();
	var icon_url = $("#icon_url").val();
	var description = $("#description").val();

	socket_handler.send_socket({controller:"team_public", action:"create",
				    name: name,
				    icon_url: icon_url,
				    description: description});
    },

    created: function(params) {
	location.href = "/shared/index.html";
    },

    all: function() {
	socket_handler.send_socket({controller:"team", action:"all"});	
    },

    alled: function(params) {
	var self = this;
	self.create_team_views(params.teams);
    },

    list: function() {
	socket_handler.send_socket({controller:"team", action:"list"});
    },

    listed: function(params) {
	var self = this;
	self.create_team_views(params.teams);
    },

    add_usr: function(team_id) {
	socket_handler.send_socket({controller:"team", action:"add_usr",
				    team_id:team_id});
    },

    usr_added:function(params) {
	if (params.result) {
	    alert("参加しました！");
	} else {
	    alert("エラーが発生しました");
	}
    },

    checkin: function(team_id) {
	socket_handler.send_socket({controller:"team", action:"checkin",
				    team_id:team_id});
    },

    checkedin: function(params) {
	location.href = "/shared/checkin_team.html";
    },

    show_checkin: function() {
	socket_handler.send_socket({controller:"team", action:"show_checkin"});
    },

    show_checkedin: function(params) {
	$("#team-name").html(params.team.name);
    },

    create_and_add_team_views: function(team_list) {
	var template = "<h3>@name@</h3><p>@description@</p>";
	template += "<a href='javascript:void(0)' onclick='team.add_usr(@team_id@)'>参加する</a><br />";
	template += "<a href='javascript:void(0)' onclick='team.checkin(@team_id@)'>チェックイン</a><br />";

	$("#all-list ul").html("");

	for (var i=0; i < team_list.length; i++) {
	    var team = team_list[i];
	    var team_box = template;
	    team_box = team_box.replace("@name@", team.name);
	    team_box = team_box.replace("@description@", team.description);
	    team_box = team_box.replace("@team_id@", team.id);
	    team_box = team_box.replace("@team_id@", team.id);
	    $("#all-list ul").append("<li>" + team_box + "</li>");
	}
    },

    create_team_views: function(team_list) {
	var template = 
	    "<li><img src='images/cromos.jpg' alt='' /> <ul class='members'> <li><img src='users/hiroe_orz17.jpg' /></li> <li><img src='users/naokaj.jpg' /></li> <li><img src='users/525un.png' /></li> <li><img src='users/y_shibata.jpg' /></li> </ul><div style='clear:both;'></div> <h3>@name@</h3> <p>@description@</p> </li>";
	
	$("ul #teamlist").html("");

	for (var i=0; i < team_list.length; i++) {
	    var team = team_list[i];
	    var team_box = template;
	    team_box = team_box.replace("@name@", team.name);
	    team_box = team_box.replace("@description@", team.description);
	    team_box = team_box.replace("@team_id@", team.id);
	    team_box = team_box.replace("@team_id@", team.id);
	    $("#teamlist").append(team_box);
	}
	
    }
}

var team = new Team;