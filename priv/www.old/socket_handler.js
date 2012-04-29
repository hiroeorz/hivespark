function SocketHandler() {}
var SocketHandler = function() {this.init();}

SocketHandler.prototype = {

    init: function() {
	this.server_uri = "ws://localhost:8080/websocket";
	this.socket = null;
    },

    open_connection: function() {
	var self = this;

	if ("WebSocket" in window) {
            self.socket = new WebSocket(self.server_uri);
	} else if ("MozWebSocket" in window) {
            self.socket = new MozWebSocket(self.server_uri);
	}
    
	if (self.socket) {

            self.socket.onopen = function() {// websocket is connected
		send_socket({controller: "chat", action: "init"});
            };
	
            self.socket.onmessage = function (evt) {
		if (evt.data == "Tick"){ return; }
		//alert("you got message!(" + evt.data + ")");
	    
		var data = evt.data;
		var msg = JSON.parse(data);
	    
		if (msg == undefined) { return; }
	    
		if (msg.message != undefined) {
		    addStatus("recv: " + msg.message);
		    return;
		}
	    
		if (msg.status != undefined && msg.message != undefined) {
		    addStatus("status: " + msg.message);
		    return;
		}
	    
		if (msg.result != undefined && msg.result == "error") {
		    alert("エラー: " + msg.reason);
		    return;
		}
	    
		addStatus("Undefined message: " + data);
            };
	    
            self.socket.onclose = function() {// websocket was closed
		addStatus("websocket was closed");
            }
	
	} else {// browser does not support websockets
            addStatus("sorry, your browser does not support websockets.");
	}	
    },

    send_socket: function(obj) {
	self.socket.send(JSON.stringify(obj));
	$("#message").val("");
    },

    send_message: function() {
	var textMessage = $("#message").val();
	send_socket({controller:"message", action:"send", text: textMessage});
    }
}

var socket_handler = new SocketHandler;