var HS = HS || {};

HS.MessageQueue = [];

/**
 * websocketによるシステムからのノティフィケーション
 *
 * @namespace HS
 * @class notification
*/

HS.Notification = function () { 'use strict'; this.init(); };

HS.Notification.prototype = (function () {
    'use strict';

    /* private */
    var uri = "ws://" + location.host + "/websock/notification";
    var websock = undefined;
    var not_support_msg = "プッシュ通知サービスはサポートされません";
    var websock_shoutdown_msg = "(;･ิω･ิ)サーバとの通信が途切れました";
    var reconnect_interval = 5000;
    var keep_alive_interval = 60000;

    var open_connection = function(mode) {
	var self = this;

	if ("WebSocket" in window) {
	    websock = new WebSocket(uri);
	} else if ("MozWebSocket" in window) {
	    websock = new MozWebSocket(uri);
	}
	
	if (websock) { // browser supports websockets
	    websock.onopen = handle_open;
	    websock.onmessage = handle_event;
	    websock.onclose = handle_close;
	    setTimeout(function() { keep_alive(); }, keep_alive_interval);

	} else { // browser does not support websockets
	    debug_message(self.not_support_msg);

	    if (mode == "retry") {
		setTimeout(function() { open_connection("retry"); }, 
			   reconnect_interval);
	    }
	}	
    };

    var debug_message = function(msg) { /* alert(msg); */ };

    /**
     * サーバとの接続がオープンしたら呼ばれる
     *
     * @method handle_close
     */    
    var handle_open = function() {
    };

    /**
     * サーバとの接続が遮断されたら呼ばれる
     *
     * @method handle_close
     */    
    var handle_close = function() {
	debug_message(websock_shoutdown_msg);
	setTimeout(function() { open_connection("retry"); }, 
		   reconnect_interval);
    };

    /**
     * サーバからデータが送信されたら呼ばれる
     *
     * @method handle_event
     */    
    var handle_event = function(evt) {
	var msg_data = evt.data; debug_message(msg_data);
	var msg = JSON.parse(msg_data);
	HS.MessageQueue.push(msg);
	
	if (msg.type == "keep_alive") { handle_keep_alive(msg); }
	if (msg.type == "message") { handle_message(msg); }
    };
 
    /**
     * ユーザへののティフィケーションメッセージを受け取った時の処理
     *
     * @method handle_message
     */
    var handle_message = function(msg) {
	var view = create_notification_view(msg.message);
	$("div.notification-message").remove();
	$("div#header").append(view);
	$(view).fadeIn("normal");

	setTimeout(function() {
	    $(view).animate({opacity:0.5}, 2000);
	}, 10000)
    };

    /**
     * 一定時間通信しないと接続遮断するルータ対策に時々通信しておく
     *
     * @method keep_alive
     */
    var keep_alive = function() {
	if (websock) {
	    websock.send("TICK");
	    setTimeout(function() { keep_alive(); }, keep_alive_interval);
	}
    };

    /**
     * keep_aliveでデータ送信した応答確認用関数。今はなにもしない
     *
     * @method handle_keep_alive
     */
    var handle_keep_alive = function(msg) {
    }

    /**
     * メッセージ要素のビューを作って返す
     *
     * @method create_notification_view
     */
    var max_message_len = 30;
    var create_notification_view = function(message) {
	var div = document.createElement("div");
	div.className = "notification-message";

	var img = document.createElement("img");
	img.src = message.usr.icon_url;
	img.alt = message.usr.name;

	var sub_text = message.text;
	if (sub_text.length > max_message_len) { 
	    sub_text = sub_text.substr(0, max_message_len - 2) + "..."; 
	}

	var p = document.createElement("p");
	var text = document.createTextNode(sub_text);
	p.appendChild(text);

	var clear_div1 = document.createElement("div");
	clear_div1.style.cssText = "clear:both;";

	var name_h3 = document.createElement("h3");
	var name_text = document.createTextNode(message.usr.name);
	name_h3.appendChild(name_text);

	var time_p = document.createElement("p");
	time_p.className = "tweet_time";
	var time_text = document.createTextNode(message.created_at);
	time_p.appendChild(time_text);

	var reply_a = document.createElement("a");
	reply_a.href = "#";

	var clear_div2 = document.createElement("div");
	clear_div2.style.cssText = "clear:both;";
	
	div.appendChild(img);
	div.appendChild(p);
	div.appendChild(clear_div1);
	div.appendChild(name_h3);
	div.appendChild(time_p);
	div.appendChild(reply_a);
	div.appendChild(clear_div2);

	return div;
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

	start: function() {
	    open_connection();
	}
    };
}());
