"""
Program checks if given email address is disposable
"""
import requests

email = input("Enter email address: ")

response = requests.get('https://www.disify.com/api/email/' + email)

# check if the request was successful (status code 200)
if response.status_code == 200:
    data = response.json()  # convert response to JSON format
    print("disposable" if data.get('disposable') else "not disposable")
else:
    print('Error:', response.status_code)


"""TESTS:
isz21863@zslsz.com (from https://10minutemail.net/)
rektor@uwr.edu.pl
"""