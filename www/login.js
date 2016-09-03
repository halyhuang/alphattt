var serviceURL = "alphattt.yaws";
var methods = ["login"];

var jsonrpc = imprt("jsonrpc");
var service = new jsonrpc.ServiceProxy(serviceURL, methods);


/*window.onload = function() {  
	init_botton();	
};  

function init_botton()
{
    var button = document.getElementById('button');
	button.onclick = login;
}  

function login()
{
	var userName = document.getElementById('username').value;
	var password = document.getElementById('password').value;
	
	var result = service.login(userName, password);
	if (!result)
	{
		alert("username or password is not exist");
		return;
	}
	window.location.href="alphattt.html"; 
}
*/



$(function()
{
    $("#login").click(function()
    {
		var userName = $("#username").val();
		var password = $("#pw").val();

		var result = service.login(userName, password);
        if (!result.result)
        {
            alert(result.reason);
            return;
        }
		$.cookie("username", userName);
		$.cookie("password", password);
        window.location.href="gamehall.html"; 
    });

    $("#sub").click(function () {
			var username = $("#username").val();
			var pw = $("#pw").val();
            if(username == "" || pw == "")
            {
                alert("用户名或密码不能为空！");
            }
		});

});
