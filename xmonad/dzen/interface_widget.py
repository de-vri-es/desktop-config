from pyroute2 import IPRoute

# get access to the netlink socket
ip = IPRoute()

# print interfaces
print(ip.get_links())

# release Netlink socket
ip.close()

