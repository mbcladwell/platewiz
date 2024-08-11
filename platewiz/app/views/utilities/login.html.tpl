<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html>
  <head>
 <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <title>LIMS*Nucleus </title>
    <link rel="alternate" type="application/atom+xml" title="Atom 1.0" href="atom/1" />
    
   <link rel="stylesheet" type="text/css" media="screen" href="../css/common.css" /> 
    
  </head>
<!-- Side navigation -->
<div class="sidenav">
<style>
img {
  display: block;
  margin-left: 12%;
  margin-right: auto;
}
</style>
</div>
<img src="../img/las.png" alt="Laboratory Automation Solutions" style="width:500px;height:180px;">

<div class="main">

    
 
<h1>Login to LIMS*Nucleus</h1>

<h3>Version: <%= version %></h3>
<h3>Connection: <%= nopwd-conn %></h3>

  <form action="/utilities/validate?name=name$value&password=password$value">
  <label for="name">Name:</label>  <input type="text" id="name" name="name" value="" ><br>
  <label for="password">Password:</label>  <input type="text" id="password" name="password" value="" ><br><br>
  <input type="submit" value="Submit">

  <!-- 
  <p style="visibility: hidden;">
 <input type="text" id="id" name="id" value="" >
   </p>
  -->
  
  </form> 



</div>
</html>
