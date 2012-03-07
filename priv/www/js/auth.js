function Auth() {}
var Auth = function() {this.init();}

Auth.prototype = {

    init: function() {
	this.server_uri = "ws://localhost:8080/websocket";
	this.socket = null;
    },

    login: function() {
	var self = this;
	var username = $("#username").val();
	var password = $("#password").val();
	socket_handler.send_socket({controller:"auth", action:"login",
				    username:username, password:password});
    },

    loggedin: function(msg) {
	if (msg.status == true) {
	    socket_handler.add_status(msg.message);
	    $.cookie("usr_id", msg.usr_id);
	    $.cookie("session_key", msg.session_key);
	    location.href = "/shared/team_list.html";
	} else {
	    socket_handler.add_status("認証に失敗しました");
	}
    }
}

var auth = new Auth;