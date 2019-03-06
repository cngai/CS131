import asyncio # for concurrent code async/await syntax
import time # for POSIX time accesss and conversions
import aiohttp # for async HTTP client/server

# port allocation numbers 11790-11794

# assign server port numbers
SERVERS = {
	'Goloman': 11790,
	'Hands': 11791,
	'Holiday': 11792,
	'Welsh': 11793,
	'Wilkes': 11794
}

# handle all types of requests

async def handle_requests(serv_name, req, line_list):


# main function

# global variables
serv_name = ""
loop = ""
async_tasks = {}	# dictionary to hold async tasks

# iterate through lines of reader and handle requests 
async def handle_reader(r, w):
	# loop through reader
	while not r.at_eof():
		line = await r.readline()
		dec_line = line.decode(encoding='UTF-8', errors='strict') # decode line with UTF-8 encoding
		line_list = dec_line.split(' ')	# put words in line into list
		await handle_requests(w, dec_line, line_list)

# accept client and create asyncio task
def handle_queries(r, w):
	t = asyncio.create_task(handle_reader(r, w)) # if that doesn't work try asyncio.ensure_future(obj)
	async_tasks[t] = (r, w) # put task in dictionary

	# close client when finished
	def close_task():
		del async_tasks[task]
		w.close()	# close writer

	# close client when task finished running
	t.add_done_callback(close_task)

def main():
	# make sure only 2 arguments
	if len(sys.argv) != 2:
		print("ERROR: incorrect number of arguments")
		exit(1)

	# ensure valid server name
	serv_name = sys.argv[1]
	if serv_name not in SERVERS:
		print("ERROR: invalid server name")
		exit(1)

	# get current event loop
	event_loop = asyncio.get_event_loop()

	# accept TCP connections from clients and create server
	address = "127.0.0.1"
	port_num = SERVERS[serv_name]
	serv_coro = asyncio.start_server(handle_queries, host=address, port=port_num, loop=event_loop) # server coroutine

	# run event loop to process events and handle client requests
	server = event_loop.run_until_complete(serv_coro)

	# close server and event loop when finished
	server.close()
	event_loop.run_until_complete(server.wait_closed()) # wait until server closed
	event_loop.close()

if __name__ == '__main__':
    main()
