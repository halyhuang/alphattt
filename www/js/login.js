var auth_jsonrpc = imprt("jsonrpc");
var auth_service = new auth_jsonrpc.ServiceProxy("auth.yaws", ["is_login", "login", "logout"]);

$(function()
{
	var islogin = is_login();
    $("#login").click(function()
    {
		if (islogin)
		{
			auth_service.logout();
		}
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
		
		var result = auth_service.login(userName, password);
        if (result.value)
        {
            location.href = "hall.html";
        }
		else
		{
			alert("登录失败！原因：" + result.reason);
		}
    });
    $("#guest").click(function()
    {		
		var result = auth_service.login("guest", "");
        if (result.value)
        {
            location.href = "hall.html";
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
                alert("用户名或密码不能为空");
            }
		});
	function is_login()
	{
		var r = auth_service.is_login();
		return r.value;
	}
});
