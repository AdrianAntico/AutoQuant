# Azure notes:
# In Microsoft Azure Machine Learning:
#   go to Datasets
#   + Create dataset - get your storage blob data into the datasets part of Microsoft Azure Machine Learning
#   Click on Registered datasets
#   Click on dataset name in list of registered datasets
#   Click on Consume
#   You will find a code snippet with subscription_id, resource_group, and workspace_name

# Command line setup
import argparse
parser = argparse.ArgumentParser(description = "Data Transfer")
parser.add_argument("subscription_id", default = '09b5fdb3-165d-4e2b-8ca0-34f998d176d5', help = "Azure subscription_id, e.g. '09b5fdb3-165d-4e2b-8ca0-34f998d176d5'")
parser.add_argument("resource_group", default = 'xCloudData', help = "Azure resource group, e.g. 'xCloudData'")
parser.add_argument("workspace_name", default = 'xCloudML', help = "Azure resource group, e.g. 'xCloudML'")
parser.add_argument("registered_dataname", help = "Name of registered dataset in azure machine learning")
parser.add_argument("csv_dataname", help = "What you want to name your csv as")
parser.add_argument("data_save_path", help = "Azure compute instance directory to save data")

# Create args
args = parser.parse_args()

# Azure module
from azureml.core import Workspace, Dataset

# Azure setup
workspace = Workspace(args.subscription_id[0], args.resource_group[0], args.workspace_name[0])
dataset = Dataset.get_by_name(workspace, name = args.registered_dataname[0])

# Convert to pandas and save as csv
df = dataset.to_pandas_dataframe()
df.to_csv(args.data_save_path[0])
