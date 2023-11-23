import asyncio
import aiohttp

async def get_calendar(country, year):
    try:
        from priv import api_key
    except:
        api_key = "kCDdwE1wTzZPK0O8TKFjWKGmSk3XfQA8"
    url = f'https://calendarific.com/api/v2/holidays?&api_key={api_key}&country={country}&year={year}'
    try:
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                if response.status != 200:
                    raise aiohttp.ClientResponseError(response.status, message=f"Unexpected status code {response.status}")
                print(await response.json())
    except Exception as e:
        print(f"An error occurred: {e}")

async def check_email(email):
    async with aiohttp.ClientSession() as session:
        url = 'https://www.disify.com/api/email/' + email
        async with session.get(url) as response:
            if response.status != 200:
                raise aiohttp.ClientResponseError(response.status, message=f"Unexpected status code {response.status}")
            data = await response.json()
            print(data.get('disposable'))


print("Holidays in US in 2019:")
asyncio.run(get_calendar("US", 2019))

print("Is email disposable?")
email = "isz21863@zslsz.com"
asyncio.run(check_email(email))

"""TESTS:
isz21863@zslsz.com (from https://10minutemail.net/)
rektor@uwr.edu.pl
"""