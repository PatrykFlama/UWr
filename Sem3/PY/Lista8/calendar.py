"""
check calendar stuff
calendarific.com
"""
import requests

api_key = 'kCDdwE1wTzZPK0O8TKFjWKGmSk3XfQA8'

response = requests.get('https://calendarific.com/api/v2/holidays?' + 
                        '&api_key=' + api_key + '&country=' + 'US' + '&year=' + '2019')

# check if the request was successful (status code 200)
if response.status_code == 200:
    data = response.json()  # convert response to JSON format
    print(data)
else:
    print('Error:', response.status_code)