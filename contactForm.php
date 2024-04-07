<?php
if ($_SERVER["REQUEST_METHOD"] == "POST") {
  $name = $_POST['name'];
  $email = $_POST['email'];
  $message = $_POST['message'];
  $to = 'avunguye@gmail.com'; 
  $subject = 'Contact Submission from Portfolio Website';
  $body = "Name: $name\nEmail: $email\nMessage: $message";

if (mail($to, $subject, $body)) {
  echo "Thank you for your submission!";
} else {
  echo "Error sending email. Please try again later.";
}
}
?>
