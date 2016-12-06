var jsonrpc_obj = imprt("jsonrpc");
 
$(function()
{
	init_websocket();
    $("#login").click(function()
    {
		var userName = $("#username").val();
		var password = $("#pw").val();

		if(userName == "" || password == "")
        {
            alert("用户名或密码不能为空");
			return;
        }
		
		if(userName == password)
        {
            alert("用户名或密码不能相同");
			return;
        }
		
        Message = construct_json_request("login", [userName, password]),
        sendMessage(Message);

    });
    $("#guest").click(function()
    {		
        Message = construct_json_request("login", ["Guset", ""]),
        sendMessage(Message);	
    });

	function init_websocket()
	{
		if(!("WebSocket" in window))
		{
			console.log("WebSocket is not support.");  
        } 
 		else 
 		{
      		var wsHost = "ws://" + window.location.host + ":" + window.location.port + "/login_ws.yaws";
            window.websocket = new WebSocket(wsHost);
            window.websocket.onopen = function(evt) { onOpen(evt) }; 
            window.websocket.onclose = function(evt) { onClose(evt) }; 
            window.websocket.onmessage = function(evt) { onMessage(evt) }; 
            window.websocket.onerror = function(evt) { onError(evt) }; 
        };
 	};

 	function sendMessage(Message)
 	{
 		if(window.websocket.readyState == websocket.OPEN)
 		{
            return window.websocket.send(Message); 
        } 
        else 
        {
        	console.log("websocket is not open");
        	return false; 
 		};
 	};

    function construct_json_request(method, args)
    {
        var p = [jsonrpc_obj.marshall("httpReq"), jsonrpc_obj.marshall(method), jsonrpc_obj.marshall(args)];
        return '{"id":' + p[0] + ', "method":' + p[1] + ', "params":' + p[2] + "}";
    }

	function onOpen(evt) 
	{ 
   		console.log("open");
    };  
	function onClose(evt) 
	{ 
        window.websocket = null;
   		console.log("onClose");
    };    
	function onMessage(evt) 
	{ 
        if (null == evt || null == evt.data)
        {
            return;
        }

        var dataObj = JSON.parse(evt.data);
        if ("login" == dataObj.method)
        {
            if (!dataObj.value)
            {
                alert("登录失败！用户名或者密码不对");
            }
        }

    };   	
	function onError(evt) 
	{ 
   		console.log("onError");
    };  
});
