var auth_jsonrpc = imprt("jsonrpc");
var auth_service = new auth_jsonrpc.ServiceProxy("auth.yaws", ["init_session", "is_login", "login"]);

function is_login()
{
	var r = auth_service.is_login();
	return r.value;
}

function init_session()
{
    try {
		auth_service.init_session();
     } catch(e) {
        alert(e);
     }	
}

window.onload = function() {  
	init_session();
};  

$(function()
{
    $("#login").click(function()
    {
		var userName = $("#username").val();
		var password = $("#pw").val();

		var result = auth_service.login(userName, password);
        if (result.value)
        {
            location.href = "alphattt.html";
        }
		else
		{
			alert("登录失败！原因：" + result.reason);
		}
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
