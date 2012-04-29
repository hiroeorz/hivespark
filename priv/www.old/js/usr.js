function Usr() {}
var Usr = function() {this.init();}

Usr.prototype = {

    init: function() {
    },

    create: function() {
	var name = $("#name").val();
	var longname = $("#longname").val();
	var password = $("#password").val();
	var email = $("#email").val();
	var icon_url = $("#icon_url").val();
	var description = $("#description").val();

	socket_handler.send_socket({controller:"usr_public", action:"create",
				    name: name,
				    longname: longname,
				    password: password,
				    email: email,
				    icon_url: icon_url,
				    description: description});
    },

    created: function(msg) {
	location.href = "/shared/login.html";
    }
}

var usr = new Usr;