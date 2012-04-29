function SocketHandler() {}
var SocketHandler = function() {this.init();}

SocketHandler.prototype = {

    init: function() {
	this.server_uri = "ws://localhost:8080/websocket";
	this.socket = null;
	this.open_callback = undefined;
    },

    opened: function(msg) {
	var self = this;
	self.add_status(msg.message);
	if (self.open_callback() != undefined) {self.open_callback();}
    },

    open_connection: function() {
	var self = this;

	if ("WebSocket" in window) {
            self.socket = new WebSocket(self.server_uri);
	} else if ("MozWebSocket" in window) {
            self.socket = new MozWebSocket(self.server_uri);
	}
    
	if (self.socket) {

            self.socket.onopen = function() {
		self.send_socket({controller: "socket", action: "init"});
            };
	
            self.socket.onmessage = function (evt) {
		if (evt.data == "Tick"){ return; }
		var data = evt.data;
		var msg = JSON.parse(data);
		if (msg == undefined) { return; }
		self.handle(msg);	    
            };
	    
            self.socket.onclose = function() {
		self.add_status("接続が閉じました");
            }
	
	} else {
            self.add_status("sorry, your browser does not support websockets.");
	}	
    },

    handle: function(msg) {
	if (msg.status == "not_authenticated") { 
	    self.add_status("セッションが途切れました")
	    return; 
	}

	if (msg.controller != undefined && msg.action != undefined) {
	    var data = JSON.stringify(msg);
	    var call_back = msg.controller + "." + msg.action + "(" + data + ")";
	    eval(call_back);
	}
    },

    send_socket: function(obj) {
	var self = this;
	var session_key = $.cookie("session_key");
	var usr_id = $.cookie("usr_id");

	if (session_key != undefined && usr_id != undefined) {
	    obj.session_key = session_key;
	    obj.usr_id = usr_id;
	}

	self.socket.send(JSON.stringify(obj));
    },

    send_message: function() {
	var self = this;
	var textMessage = $("#message").val();
	self.send_socket({controller:"message", action:"send", text: textMessage});
	$("#message").val("");
    },

    add_status: function(msg) {
	$("#status").html(msg);
    }
}

var socket_handler = new SocketHandler;