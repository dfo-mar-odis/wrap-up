import os
import tempfile
import keyring
from office365.runtime.auth.user_credential import UserCredential
from office365.sharepoint.client_context import ClientContext

# WILL NOT WORK BECAUSE OF MF-MFA.

# set the username/passwords with : keyring.exe set sharepoint username and
# keyring.exe set sharepoint password in the terminal
sharepoint_username = keyring.get_password("sharepoint", "username")
sharepoint_password = keyring.get_password("sharepoint", "password")

user_credentials = UserCredential(sharepoint_username, sharepoint_password)

sharpoint_url = "https://086gc.sharepoint.com"
ctx = ClientContext(sharpoint_url).with_credentials(user_credentials)

# to get this path, you need to open the file in the desktop app, then info->copy path
file_url = "/sites/MaritimesSpatialPlanning-MAROpenDataLogistics/Shared%20Documents/MAR%20OpenData%20Logistics/Inventories/ReproducibleReporting-Data-Metadata.xlsx"
download_path = os.path.join(tempfile.mkdtemp(), os.path.basename(file_url))
with open(download_path, "wb") as local_file:
    file = ctx.web.get_file_by_server_relative_path(file_url).download("output.xlsx").execute_query()
print("[Ok] file has been downloaded into: {0}".format(download_path))