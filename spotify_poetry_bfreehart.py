"""
File: spotify_poetry_bfreehart.py
Author: Brendan Freehart
Version: 0.1	Date: 2014-02-13
Description: My first Python program! 
	This program is intended to build a playlist of Spotify tracks such that it looks like a poem, inspired by: http://spotifypoetry.tumblr.com/
	The program prompts the user for a poem and then iterates backwards through the string, looking for a valid result from Spofity and then 
	printing out the result. Once a track is found, the words in the track are removed from the poem/input string and then the function recursively
	searches until the string is gone or yeilds no results. 

Future Enhancements: 
	1.) Organize/beautify 'pythonically' i.e. per The Python Style Guide. This will likely include, at least, breaking the __find_poem function
		into multiple classes.
	2.) The backwards iteration retrieves tracks the *mostly* match a Spotify track title. Then, it removes the track's title from the poem.
		The algorithm would be better if it were to remove every word preceding the final value found within the track's title.
	3.) Important words - Define&parse-out an array of joining words which are typically not crucial to the sentiment of a poem...
	4.) General bug testing / debugging
	5.) Optimizing the general performance
	6.) Code in authentication with Spotify's Web API and then creating a playlist with the output URLs (or URIs)
"""
import spotipy
import sys

url_string = 'http://open.spotify.com/track/'

spotify = spotipy.Spotify()

# Ask user for input
print('\nHi. Please enter a poem for your loved one and I will make it into a Spotify playlist\n')

input_str = raw_input("Enter your poem: ")

# Verify input string
if input_str > 1:
    name = input_str
else:
    name = 'Error 404' # Gives user a Martin Garrix song is user does not enter anything

# This is the main function. 
def __find_poem(phrase):
	words = phrase.split()
	# Iterate backwards through the poem, looking for longest string that will yeild a Spotify track
	for i in xrange(len(words), 0, -1):
		snip = " ".join(words[:i])
		results = spotify.search(q='track:' + snip, type='track')
		items = results['tracks']['items']
		if len(items) > 0:
			top_track = items[0]
			# Regarding "encode(sys.stdout.encoding, errors='replace')" - I found that some tracks return funky encoding. In this cases, the character is automatically replaced with a '?'
			print top_track['name'].encode(sys.stdout.encoding, errors='replace'), top_track['artists'][0]['name'].encode(sys.stdout.encoding, errors='replace'), url_string + top_track['id'].encode(sys.stdout.encoding, errors='replace')
			track_name_words = str(top_track['name'].encode(sys.stdout.encoding, errors='replace')).split()
			words = [x.lower() for x in words] # Make both strings lowercase
			track_name_words = [x.lower() for x in track_name_words]
			new_phrase = [x for x in words if x not in track_name_words] # Remove words that were found in the track
			new_phrase = " ".join(filter(None, new_phrase)) # Create new phrase
			__find_poem(new_phrase)
			break 

__find_poem(input_str)
