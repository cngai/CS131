import sys
import asyncio # for concurrent code async/await syntax
import time # for POSIX time accesss and conversions
import aiohttp # for async HTTP client/server
import json # for JSONs

# port allocation numbers 11790-11794

# create nested dict of servers to port numbers and commumication patterns
servers_dict = {
	'Goloman': { 'port': 11790, 'comm_patt': ['Hands', 'Holiday', 'Wilkes'] },
	'Hands': { 'port': 11791, 'comm_patt': ['Goloman, Wilkes'] },
	'Holiday': { 'port': 11792, 'comm_patt': ['Goloman', 'Welsh', 'Wilkes'] },
	'Welsh': { 'port': 11793, 'comm_patt': ['Holiday'] },
	'Wilkes': { 'port': 11794, 'comm_patt': ['Goloman', 'Hands', 'Holiday'] }
}

# dictionary to hold all clients
clients_dict = {}

# global variables
# serv_name = ""
#log_file = ""
# event_loop = ""
async_tasks = {}	# dictionary to hold async tasks
address = "127.0.0.1"
key = 'AIzaSyD_K5I-vj1KmbmfguQ1fM4-t7us048XaaQ'	# API key for Google API

# log input and output into file
def log_io(message):
	if message == None:
		return
	else:
		log_file.write(message)

# send response back to writer
async def send_response(w, response_message):
	encoded_message = response_message.encode('UTF-8', errors='strict')

	# write data to writer stream
	w.write(encoded_message)
	await writer.drain()	# wait until appropriate to resume writing to stream
	w.write_eof()	# close write end of stream

# flooding algorithm - propagate location updates to other servers
async def flood_to_servers(cli_id, at_response):
	print("Flooding to other servers")
	# get client name
	cli = clients_dict[cli_id]

	# send to all the connections of the server
	curr_server = cli['serv_name']
	for other_server in servers_dict[curr_server]['comm_patt']:
		try:
			# establish network connection
			r, w = await asyncio.open_connection(address, servers_dict[other_server]['port'], loop=event_loop)
			await send_response(w, at_response)
		except:
			print('ERROR: unable to propogate message to %s' % (other_server))
			await log_io('ERROR: unable to propogate message to %s' % (other_server))

# convert latitude/longitude in ISO 6709 notation into tuple of (lat, long)
def convert_lat_long(lat_long):
	latitude = ""
	longitude = ""
	done_with_lat = 1 # 0 for false, 1 for true; starts at 1 b/c immediately should switch to 0

	# iterate through each char in lat_long
	for i in lat_long:
		if i == '+' or i == '-':
			# switch
			if done_with_lat == 1:
				done_with_lat = 0
			else:
				done_with_lat = 1

		# append chars to either lat or long
		if done_with_lat == 0:
			latitude += i
		else:
			longitude += i

	return latitude, longitude


# handle IAMAT commands
async def handle_iamat(cli_id, lat_long, cli_time, start_time, w):
	print("Handling IAMAT")
	# get latitude and longitude from ISO 6709 notation
	latitude, longitude = convert_lat_long(lat_long)

	# make sure we have most updated cli_id data
	if cli_id in clients_dict:
		print ("yeet")
		if clients_dict[cli_id]['cli_time'] > cli_time:
			return None

	# define curr_cli object
	curr_cli = {
		'latitude': latitude,
		'longitude': longitude,
		'time_difference': float(cli_time) - start_time,
		'cli_time': cli_time,
		'serv_name': serv_name
	}

	# add curr_cli to clients_dict
	clients_dict[cli_id] = curr_cli

	# send back response message and flood other servrers
	at_response = "AT %s %s %s %s %s" % (serv_name, str(float(cli_time) - start_time), cli_id, lat_long, start_time)
	await flood_to_servers(cli_id, at_response)
	await send_response(w, at_response)
	await log_io('AT response to IAMAT:\n' + at_response + '\n')

# handle AT commands
# async def handle_at(serv_id, time_diff, cli_id, lat_long, cli_time, serv_name):
# 	if

# make Nearby Search request
async def make_ns_request(session, curr_cli, radius, num_results):
	latitude = curr_cli['latitude']
	longitude = curr_cli['longitude']

	# make Nearby Search Request
	request_url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=%s,%s&radius=%d&key=%s' % (latitude, longitude, float(radius), key)
	async with session.get(request_url) as response:
		ns_results = await response.json()
		# get first num_results entries in ns_results
		return ns_results["results"][:num_results]


# handle WHATSAT commands
async def handle_whatsat(cli_id, radius, upper_bound, start_time, w):
	# check if valid client
	if cli_id not in clients:
		await log_io('WHATSAT command sent by invalid client\n')
		await send_response(w, 'WHATSAT command sent by invalid client\n')

	# get current client from dict
	curr_cli = clients_dict[cli_id]
	
	# https://aiohttp.readthedocs.io/en/stable/
	async with aiohttp.ClientSession() as session:
		places_json = await make_ns_request(session, curr_cli, radius, int(upper_bound))
		places_string = json.dumps(places_json) # turn into JSON string so we can output
		print(places_string) # for debugging

		# send response message back
		at_response = "AT %s %s %s %s %s %s" % (serv_name, curr_cli['time_difference'], cli_id, curr_cli['latitude'] + curr_cli['longitude'], start_time, places_string)
		await log_io('AT response to WHATSAT:\n' + at_response + '\n')
		await send_response(w, at_response)


# handle all types of requests
async def handle_commands(line_list, req, w):
	#print ("handle_commands function")

	# get time in seconds since epoch
	start_time = time.time()

	# either IAMAT, AT, or WHATSAT
	command = line_list[0]

	# check if valid command
	if command != "IAMAT" and command != "AT" and command != "WHATSAT":
		print("ERROR: invalid command of '%s'. Now exiting." % (command))
		exit(1)

	# IAMAT
	if command == "IAMAT":
		print("Received AN IAMAT command.")
		await handle_iamat(line_list[1], line_list[2], line_list[3], start_time, w)

	# AT
	# if command == "AT":
	# 	handle_at(line_list[1], line_list[2], line_list[3], line_list[4], line_list[5], serv_name)

	# WHATSAT
	if command == "WHATSAT":
		await handle_whatsat(line_list[1], line_list[2], line_list[3], start_time, w)


# main function

# iterate through lines of reader and handle requests 
async def handle_reader(r, w):
	# loop through reader
	while not r.at_eof():
		line = await r.readline()

		# NOT SURE IF I NEED THIS
		dec_line = line.decode(encoding='UTF-8', errors='strict') # decode line with UTF-8 encoding
		line_list = dec_line.split(' ')	# put words in line into list
		await handle_commands(line_list, dec_line, w)

# accept client and create asyncio task
def handle_queries(r, w):
	# for debugging
	print("Connected")

	t = asyncio.create_task(handle_reader(r, w)) # if that doesn't work try asyncio.ensure_future(obj)
	async_tasks[t] = (r, w) # put task in dictionary

	# close client when finished

	def close_task(t):
		del async_tasks[t]
		w.close()	# close writer

	# close client when task finished running
	t.add_done_callback(close_task)

def main():
	# make sure only 2 arguments
	if len(sys.argv) != 2:
		print("ERROR: incorrect number of arguments")
		exit(1)

	# ensure valid server name
	global serv_name
	serv_name = sys.argv[1]
	if serv_name not in servers_dict:
		print("ERROR: invalid server name")
		exit(1)

	# create log file
	global log_file
	log = serv_name + "-logfile.txt"
	open(log, "w").close()
	log_file = open(log, "a") # append to log

	# get current event loop
	global event_loop
	event_loop = asyncio.get_event_loop()

	# accept TCP connections from clients and create server
	port_num = servers_dict[serv_name]['port']
	serv_coro = asyncio.start_server(handle_queries, host=address, port=port_num, loop=event_loop) # server coroutine

	# run event loop to process events and handle client requests
	server = event_loop.run_until_complete(serv_coro)

	print('Starting server on %s via port %d' % (address, port_num))

	event_loop.run_forever()

	# close server and event loop when finished
	server.close()
	event_loop.run_until_complete(server.wait_closed()) # wait until server closed
	event_loop.close()

if __name__ == '__main__':
    main()
