{*******************************************************}
{               MiTeC Common Routines                   }
{                  ADSI interface                       }
{                                                       }
{        Copyright (c) 1997-2017 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

Unit MiTeC_FPC_ActiveDs_TLB;

//  Imported ActiveDs on 4.4.2017 19:51:29 from C:\Windows\System32\activeds.tlb

interface

//  Warning: renamed record member 'String' in _ADS_CASEIGNORE_LIST to 'String_'
//  Warning: renamed record member 'Type' in __MIDL___MIDL_itf_ads_0000_0000_0005 to 'Type_'
//  Warning: renamed record member 'Type' in __MIDL___MIDL_itf_ads_0000_0000_0014 to 'Type_'
//  Warning: renamed record member 'Boolean' in __MIDL___MIDL_itf_ads_0000_0000_0017 to 'Boolean_'
//  Warning: renamed record member 'Integer' in __MIDL___MIDL_itf_ads_0000_0000_0017 to 'Integer_'
// Dependency: stdole v2 (stdole2.pas)
//  Warning: renamed property 'Class' in IADs to 'Class_'
//  Warning: 'GUID' not automatable in IADsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsdisp.Invoke
//  Warning: 'GUID' not automatable in IADsContainerdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsContainerdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsContainerdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsContainerdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsContainerdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsContainerdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsContainerdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsContainerdisp.Invoke
//  Warning: 'GUID' not automatable in IADsCollectiondisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsCollectiondisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsCollectiondisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsCollectiondisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsCollectiondisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsCollectiondisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsCollectiondisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsCollectiondisp.Invoke
//  Warning: 'GUID' not automatable in IADsMembersdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsMembersdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsMembersdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsMembersdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsMembersdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsMembersdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsMembersdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsMembersdisp.Invoke
//  Warning: renamed method 'Reset' in IADsPropertyList to 'Reset_'
//  Warning: 'GUID' not automatable in IADsPropertyListdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsPropertyListdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsPropertyListdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsPropertyListdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsPropertyListdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsPropertyListdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsPropertyListdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsPropertyListdisp.Invoke
//  Warning: renamed method 'Reset' in IADsPropertyList to 'Reset_'
//  Warning: 'GUID' not automatable in IADsPropertyEntrydisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsPropertyEntrydisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsPropertyEntrydisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsPropertyEntrydisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsPropertyEntrydisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsPropertyEntrydisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsPropertyEntrydisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsPropertyEntrydisp.Invoke
//  Warning: renamed property 'Boolean' in IADsPropertyValue to 'Boolean_'
//  Warning: renamed property 'Integer' in IADsPropertyValue to 'Integer_'
//  Warning: 'GUID' not automatable in IADsPropertyValuedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsPropertyValuedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsPropertyValuedisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsPropertyValuedisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsPropertyValuedisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsPropertyValuedisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsPropertyValuedisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsPropertyValuedisp.Invoke
//  Warning: 'GUID' not automatable in IADsPropertyValue2disp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsPropertyValue2disp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsPropertyValue2disp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsPropertyValue2disp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsPropertyValue2disp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsPropertyValue2disp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsPropertyValue2disp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsPropertyValue2disp.Invoke
//  Warning: 'GUID' not automatable in IADsDeleteOpsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsDeleteOpsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsDeleteOpsdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsDeleteOpsdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsDeleteOpsdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsDeleteOpsdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsDeleteOpsdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsDeleteOpsdisp.Invoke
//  Warning: 'GUID' not automatable in IADsNamespacesdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsNamespacesdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsNamespacesdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsNamespacesdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsNamespacesdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsNamespacesdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsNamespacesdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsNamespacesdisp.Invoke
//  Warning: renamed property 'Class' in IADsNamespaces to 'Class_'
//  Warning: 'GUID' not automatable in IADsClassdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsClassdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsClassdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsClassdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsClassdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsClassdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsClassdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsClassdisp.Invoke
//  Warning: renamed property 'Class' in IADsClass to 'Class_'
//  Warning: 'GUID' not automatable in IADsPropertydisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsPropertydisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsPropertydisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsPropertydisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsPropertydisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsPropertydisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsPropertydisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsPropertydisp.Invoke
//  Warning: renamed property 'Class' in IADsProperty to 'Class_'
//  Warning: 'GUID' not automatable in IADsSyntaxdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsSyntaxdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsSyntaxdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsSyntaxdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsSyntaxdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsSyntaxdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsSyntaxdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsSyntaxdisp.Invoke
//  Warning: renamed property 'Class' in IADsSyntax to 'Class_'
//  Warning: 'GUID' not automatable in IADsLocalitydisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsLocalitydisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsLocalitydisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsLocalitydisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsLocalitydisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsLocalitydisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsLocalitydisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsLocalitydisp.Invoke
//  Warning: renamed property 'Class' in IADsLocality to 'Class_'
//  Warning: 'GUID' not automatable in IADsOdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsOdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsOdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsOdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsOdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsOdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsOdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsOdisp.Invoke
//  Warning: renamed property 'Class' in IADsO to 'Class_'
//  Warning: 'GUID' not automatable in IADsOUdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsOUdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsOUdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsOUdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsOUdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsOUdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsOUdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsOUdisp.Invoke
//  Warning: renamed property 'Class' in IADsOU to 'Class_'
//  Warning: 'GUID' not automatable in IADsDomaindisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsDomaindisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsDomaindisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsDomaindisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsDomaindisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsDomaindisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsDomaindisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsDomaindisp.Invoke
//  Warning: renamed property 'Class' in IADsDomain to 'Class_'
//  Warning: 'GUID' not automatable in IADsComputerdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsComputerdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsComputerdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsComputerdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsComputerdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsComputerdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsComputerdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsComputerdisp.Invoke
//  Warning: renamed property 'Class' in IADsComputer to 'Class_'
//  Warning: 'GUID' not automatable in IADsComputerOperationsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsComputerOperationsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsComputerOperationsdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsComputerOperationsdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsComputerOperationsdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsComputerOperationsdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsComputerOperationsdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsComputerOperationsdisp.Invoke
//  Warning: renamed property 'Class' in IADsComputerOperations to 'Class_'
//  Warning: 'GUID' not automatable in IADsGroupdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsGroupdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsGroupdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsGroupdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsGroupdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsGroupdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsGroupdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsGroupdisp.Invoke
//  Warning: renamed property 'Class' in IADsGroup to 'Class_'
//  Warning: 'GUID' not automatable in IADsUserdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsUserdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsUserdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsUserdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsUserdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsUserdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsUserdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsUserdisp.Invoke
//  Warning: renamed property 'Class' in IADsUser to 'Class_'
//  Warning: 'GUID' not automatable in IADsPrintQueuedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsPrintQueuedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsPrintQueuedisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsPrintQueuedisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsPrintQueuedisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsPrintQueuedisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsPrintQueuedisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsPrintQueuedisp.Invoke
//  Warning: renamed property 'Class' in IADsPrintQueue to 'Class_'
//  Warning: 'GUID' not automatable in IADsPrintQueueOperationsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsPrintQueueOperationsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsPrintQueueOperationsdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsPrintQueueOperationsdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsPrintQueueOperationsdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsPrintQueueOperationsdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsPrintQueueOperationsdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsPrintQueueOperationsdisp.Invoke
//  Warning: renamed property 'Class' in IADsPrintQueueOperations to 'Class_'
//  Warning: 'GUID' not automatable in IADsPrintJobdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsPrintJobdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsPrintJobdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsPrintJobdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsPrintJobdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsPrintJobdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsPrintJobdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsPrintJobdisp.Invoke
//  Warning: renamed property 'Class' in IADsPrintJob to 'Class_'
//  Warning: 'GUID' not automatable in IADsPrintJobOperationsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsPrintJobOperationsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsPrintJobOperationsdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsPrintJobOperationsdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsPrintJobOperationsdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsPrintJobOperationsdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsPrintJobOperationsdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsPrintJobOperationsdisp.Invoke
//  Warning: renamed property 'Class' in IADsPrintJobOperations to 'Class_'
//  Warning: 'GUID' not automatable in IADsServicedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsServicedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsServicedisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsServicedisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsServicedisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsServicedisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsServicedisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsServicedisp.Invoke
//  Warning: renamed property 'Class' in IADsService to 'Class_'
//  Warning: renamed method 'Continue' in IADsServiceOperations to 'Continue_'
//  Warning: 'GUID' not automatable in IADsServiceOperationsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsServiceOperationsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsServiceOperationsdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsServiceOperationsdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsServiceOperationsdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsServiceOperationsdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsServiceOperationsdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsServiceOperationsdisp.Invoke
//  Warning: renamed property 'Class' in IADsServiceOperations to 'Class_'
//  Warning: renamed method 'Continue' in IADsServiceOperations to 'Continue_'
//  Warning: 'GUID' not automatable in IADsFileServicedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsFileServicedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsFileServicedisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsFileServicedisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsFileServicedisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsFileServicedisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsFileServicedisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsFileServicedisp.Invoke
//  Warning: renamed property 'Class' in IADsFileService to 'Class_'
//  Warning: 'GUID' not automatable in IADsFileServiceOperationsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsFileServiceOperationsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsFileServiceOperationsdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsFileServiceOperationsdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsFileServiceOperationsdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsFileServiceOperationsdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsFileServiceOperationsdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsFileServiceOperationsdisp.Invoke
//  Warning: renamed property 'Class' in IADsFileServiceOperations to 'Class_'
//  Warning: renamed method 'Continue' in IADsFileServiceOperations to 'Continue_'
//  Warning: 'GUID' not automatable in IADsFileSharedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsFileSharedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsFileSharedisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsFileSharedisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsFileSharedisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsFileSharedisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsFileSharedisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsFileSharedisp.Invoke
//  Warning: renamed property 'Class' in IADsFileShare to 'Class_'
//  Warning: 'GUID' not automatable in IADsSessiondisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsSessiondisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsSessiondisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsSessiondisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsSessiondisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsSessiondisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsSessiondisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsSessiondisp.Invoke
//  Warning: renamed property 'Class' in IADsSession to 'Class_'
//  Warning: 'GUID' not automatable in IADsResourcedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsResourcedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsResourcedisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsResourcedisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsResourcedisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsResourcedisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsResourcedisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsResourcedisp.Invoke
//  Warning: renamed property 'Class' in IADsResource to 'Class_'
//  Warning: 'GUID' not automatable in IADsOpenDSObjectdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsOpenDSObjectdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsOpenDSObjectdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsOpenDSObjectdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsOpenDSObjectdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsOpenDSObjectdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsOpenDSObjectdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsOpenDSObjectdisp.Invoke
//  Warning: 'GUID' not automatable in IADsAccessControlEntrydisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsAccessControlEntrydisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsAccessControlEntrydisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsAccessControlEntrydisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsAccessControlEntrydisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsAccessControlEntrydisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsAccessControlEntrydisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsAccessControlEntrydisp.Invoke
//  Warning: 'GUID' not automatable in IADsAccessControlListdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsAccessControlListdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsAccessControlListdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsAccessControlListdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsAccessControlListdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsAccessControlListdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsAccessControlListdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsAccessControlListdisp.Invoke
//  Warning: 'GUID' not automatable in IADsSecurityDescriptordisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsSecurityDescriptordisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsSecurityDescriptordisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsSecurityDescriptordisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsSecurityDescriptordisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsSecurityDescriptordisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsSecurityDescriptordisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsSecurityDescriptordisp.Invoke
//  Warning: 'GUID' not automatable in IADsLargeIntegerdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsLargeIntegerdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsLargeIntegerdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsLargeIntegerdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsLargeIntegerdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsLargeIntegerdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsLargeIntegerdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsLargeIntegerdisp.Invoke
//  Warning: renamed method 'Set' in IADsNameTranslate to 'Set_'
//  Warning: 'GUID' not automatable in IADsNameTranslatedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsNameTranslatedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsNameTranslatedisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsNameTranslatedisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsNameTranslatedisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsNameTranslatedisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsNameTranslatedisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsNameTranslatedisp.Invoke
//  Warning: renamed method 'Set' in IADsNameTranslate to 'Set_'
//  Warning: 'GUID' not automatable in IADsCaseIgnoreListdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsCaseIgnoreListdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsCaseIgnoreListdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsCaseIgnoreListdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsCaseIgnoreListdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsCaseIgnoreListdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsCaseIgnoreListdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsCaseIgnoreListdisp.Invoke
//  Warning: 'GUID' not automatable in IADsFaxNumberdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsFaxNumberdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsFaxNumberdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsFaxNumberdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsFaxNumberdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsFaxNumberdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsFaxNumberdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsFaxNumberdisp.Invoke
//  Warning: 'GUID' not automatable in IADsNetAddressdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsNetAddressdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsNetAddressdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsNetAddressdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsNetAddressdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsNetAddressdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsNetAddressdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsNetAddressdisp.Invoke
//  Warning: 'GUID' not automatable in IADsOctetListdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsOctetListdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsOctetListdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsOctetListdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsOctetListdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsOctetListdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsOctetListdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsOctetListdisp.Invoke
//  Warning: renamed property 'Type' in IADsEmail to 'Type_'
//  Warning: 'GUID' not automatable in IADsEmaildisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsEmaildisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsEmaildisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsEmaildisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsEmaildisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsEmaildisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsEmaildisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsEmaildisp.Invoke
//  Warning: renamed property 'Type' in IADsPath to 'Type_'
//  Warning: 'GUID' not automatable in IADsPathdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsPathdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsPathdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsPathdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsPathdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsPathdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsPathdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsPathdisp.Invoke
//  Warning: 'GUID' not automatable in IADsReplicaPointerdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsReplicaPointerdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsReplicaPointerdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsReplicaPointerdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsReplicaPointerdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsReplicaPointerdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsReplicaPointerdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsReplicaPointerdisp.Invoke
//  Warning: 'GUID' not automatable in IADsAcldisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsAcldisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsAcldisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsAcldisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsAcldisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsAcldisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsAcldisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsAcldisp.Invoke
//  Warning: 'GUID' not automatable in IADsTimestampdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsTimestampdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsTimestampdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsTimestampdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsTimestampdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsTimestampdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsTimestampdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsTimestampdisp.Invoke
//  Warning: 'GUID' not automatable in IADsPostalAddressdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsPostalAddressdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsPostalAddressdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsPostalAddressdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsPostalAddressdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsPostalAddressdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsPostalAddressdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsPostalAddressdisp.Invoke
//  Warning: 'GUID' not automatable in IADsBackLinkdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsBackLinkdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsBackLinkdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsBackLinkdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsBackLinkdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsBackLinkdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsBackLinkdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsBackLinkdisp.Invoke
//  Warning: 'GUID' not automatable in IADsTypedNamedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsTypedNamedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsTypedNamedisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsTypedNamedisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsTypedNamedisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsTypedNamedisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsTypedNamedisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsTypedNamedisp.Invoke
//  Warning: 'GUID' not automatable in IADsHolddisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsHolddisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsHolddisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsHolddisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsHolddisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsHolddisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsHolddisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsHolddisp.Invoke
//  Warning: 'GUID' not automatable in IADsObjectOptionsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsObjectOptionsdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsObjectOptionsdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsObjectOptionsdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsObjectOptionsdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsObjectOptionsdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsObjectOptionsdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsObjectOptionsdisp.Invoke
//  Warning: renamed method 'Set' in IADsPathname to 'Set_'
//  Warning: 'GUID' not automatable in IADsPathnamedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsPathnamedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsPathnamedisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsPathnamedisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsPathnamedisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsPathnamedisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsPathnamedisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsPathnamedisp.Invoke
//  Warning: renamed method 'Set' in IADsPathname to 'Set_'
//  Warning: 'GUID' not automatable in IADsADSystemInfodisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsADSystemInfodisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsADSystemInfodisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsADSystemInfodisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsADSystemInfodisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsADSystemInfodisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsADSystemInfodisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsADSystemInfodisp.Invoke
//  Warning: 'GUID' not automatable in IADsWinNTSystemInfodisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsWinNTSystemInfodisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsWinNTSystemInfodisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsWinNTSystemInfodisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsWinNTSystemInfodisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsWinNTSystemInfodisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsWinNTSystemInfodisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsWinNTSystemInfodisp.Invoke
//  Warning: 'GUID' not automatable in IADsDNWithBinarydisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsDNWithBinarydisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsDNWithBinarydisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsDNWithBinarydisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsDNWithBinarydisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsDNWithBinarydisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsDNWithBinarydisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsDNWithBinarydisp.Invoke
//  Warning: 'GUID' not automatable in IADsDNWithStringdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsDNWithStringdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsDNWithStringdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsDNWithStringdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsDNWithStringdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsDNWithStringdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsDNWithStringdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsDNWithStringdisp.Invoke
//  Warning: 'GUID' not automatable in IADsSecurityUtilitydisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsSecurityUtilitydisp.QueryInterface
//  Warning: 'Ppointer' not automatable in IADsSecurityUtilitydisp.GetTypeInfo
//  Warning: 'GUID' not automatable in IADsSecurityUtilitydisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in IADsSecurityUtilitydisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in IADsSecurityUtilitydisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in IADsSecurityUtilitydisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in IADsSecurityUtilitydisp.Invoke
Uses
  Windows,ActiveX,Classes,Variants,stdole2;
Const
  ActiveDsMajorVersion = 1;
  ActiveDsMinorVersion = 0;
  ActiveDsLCID = 0;
  LIBID_ActiveDs : TGUID = '{97D25DB0-0363-11CF-ABC4-02608C9E7553}';

  IID_IADs : TGUID = '{FD8256D0-FD15-11CE-ABC4-02608C9E7553}';
  IID_IADsContainer : TGUID = '{001677D0-FD16-11CE-ABC4-02608C9E7553}';
  IID_IADsCollection : TGUID = '{72B945E0-253B-11CF-A988-00AA006BC149}';
  IID_IADsMembers : TGUID = '{451A0030-72EC-11CF-B03B-00AA006E0975}';
  IID_IADsPropertyList : TGUID = '{C6F602B6-8F69-11D0-8528-00C04FD8D503}';
  IID_IADsPropertyEntry : TGUID = '{05792C8E-941F-11D0-8529-00C04FD8D503}';
  CLASS_PropertyEntry : TGUID = '{72D3EDC2-A4C4-11D0-8533-00C04FD8D503}';
  IID_IADsPropertyValue : TGUID = '{79FA9AD0-A97C-11D0-8534-00C04FD8D503}';
  IID_IADsPropertyValue2 : TGUID = '{306E831C-5BC7-11D1-A3B8-00C04FB950DC}';
  CLASS_PropertyValue : TGUID = '{7B9E38B0-A97C-11D0-8534-00C04FD8D503}';
  IID_IPrivateDispatch : TGUID = '{86AB4BBE-65F6-11D1-8C13-00C04FD8D503}';
  IID_ITypeInfo : TGUID = '{00020401-0000-0000-C000-000000000046}';
  IID_ITypeComp : TGUID = '{00020403-0000-0000-C000-000000000046}';
  IID_ITypeLib : TGUID = '{00020402-0000-0000-C000-000000000046}';
  IID_IPrivateUnknown : TGUID = '{89126BAB-6EAD-11D1-8C18-00C04FD8D503}';
  IID_IADsExtension : TGUID = '{3D35553C-D2B0-11D1-B17B-0000F87593A0}';
  IID_IADsDeleteOps : TGUID = '{B2BD0902-8878-11D1-8C21-00C04FD8D503}';
  IID_IADsNamespaces : TGUID = '{28B96BA0-B330-11CF-A9AD-00AA006BC149}';
  IID_IADsClass : TGUID = '{C8F93DD0-4AE0-11CF-9E73-00AA004A5691}';
  IID_IADsProperty : TGUID = '{C8F93DD3-4AE0-11CF-9E73-00AA004A5691}';
  IID_IADsSyntax : TGUID = '{C8F93DD2-4AE0-11CF-9E73-00AA004A5691}';
  IID_IADsLocality : TGUID = '{A05E03A2-EFFE-11CF-8ABC-00C04FD8D503}';
  IID_IADsO : TGUID = '{A1CD2DC6-EFFE-11CF-8ABC-00C04FD8D503}';
  IID_IADsOU : TGUID = '{A2F733B8-EFFE-11CF-8ABC-00C04FD8D503}';
  IID_IADsDomain : TGUID = '{00E4C220-FD16-11CE-ABC4-02608C9E7553}';
  IID_IADsComputer : TGUID = '{EFE3CC70-1D9F-11CF-B1F3-02608C9E7553}';
  IID_IADsComputerOperations : TGUID = '{EF497680-1D9F-11CF-B1F3-02608C9E7553}';
  IID_IADsGroup : TGUID = '{27636B00-410F-11CF-B1FF-02608C9E7553}';
  IID_IADsUser : TGUID = '{3E37E320-17E2-11CF-ABC4-02608C9E7553}';
  IID_IADsPrintQueue : TGUID = '{B15160D0-1226-11CF-A985-00AA006BC149}';
  IID_IADsPrintQueueOperations : TGUID = '{124BE5C0-156E-11CF-A986-00AA006BC149}';
  IID_IADsPrintJob : TGUID = '{32FB6780-1ED0-11CF-A988-00AA006BC149}';
  IID_IADsPrintJobOperations : TGUID = '{9A52DB30-1ECF-11CF-A988-00AA006BC149}';
  IID_IADsService : TGUID = '{68AF66E0-31CA-11CF-A98A-00AA006BC149}';
  IID_IADsServiceOperations : TGUID = '{5D7B33F0-31CA-11CF-A98A-00AA006BC149}';
  IID_IADsFileService : TGUID = '{A89D1900-31CA-11CF-A98A-00AA006BC149}';
  IID_IADsFileServiceOperations : TGUID = '{A02DED10-31CA-11CF-A98A-00AA006BC149}';
  IID_IADsFileShare : TGUID = '{EB6DCAF0-4B83-11CF-A995-00AA006BC149}';
  IID_IADsSession : TGUID = '{398B7DA0-4AAB-11CF-AE2C-00AA006EBFB9}';
  IID_IADsResource : TGUID = '{34A05B20-4AAB-11CF-AE2C-00AA006EBFB9}';
  IID_IADsOpenDSObject : TGUID = '{DDF2891E-0F9C-11D0-8AD4-00C04FD8D503}';
  IID_IDirectoryObject : TGUID = '{E798DE2C-22E4-11D0-84FE-00C04FD8D503}';
  IID_IDirectorySearch : TGUID = '{109BA8EC-92F0-11D0-A790-00C04FD8D5A8}';
  IID_IDirectorySchemaMgmt : TGUID = '{75DB3B9C-A4D8-11D0-A79C-00C04FD8D5A8}';
  IID_IADsAggregatee : TGUID = '{1346CE8C-9039-11D0-8528-00C04FD8D503}';
  IID_IADsAggregator : TGUID = '{52DB5FB0-941F-11D0-8529-00C04FD8D503}';
  IID_IADsAccessControlEntry : TGUID = '{B4F3A14C-9BDD-11D0-852C-00C04FD8D503}';
  CLASS_AccessControlEntry : TGUID = '{B75AC000-9BDD-11D0-852C-00C04FD8D503}';
  IID_IADsAccessControlList : TGUID = '{B7EE91CC-9BDD-11D0-852C-00C04FD8D503}';
  CLASS_AccessControlList : TGUID = '{B85EA052-9BDD-11D0-852C-00C04FD8D503}';
  IID_IADsSecurityDescriptor : TGUID = '{B8C787CA-9BDD-11D0-852C-00C04FD8D503}';
  CLASS_SecurityDescriptor : TGUID = '{B958F73C-9BDD-11D0-852C-00C04FD8D503}';
  IID_IADsLargeInteger : TGUID = '{9068270B-0939-11D1-8BE1-00C04FD8D503}';
  CLASS_LargeInteger : TGUID = '{927971F5-0939-11D1-8BE1-00C04FD8D503}';
  IID_IADsNameTranslate : TGUID = '{B1B272A3-3625-11D1-A3A4-00C04FB950DC}';
  CLASS_NameTranslate : TGUID = '{274FAE1F-3626-11D1-A3A4-00C04FB950DC}';
  IID_IADsCaseIgnoreList : TGUID = '{7B66B533-4680-11D1-A3B4-00C04FB950DC}';
  CLASS_CaseIgnoreList : TGUID = '{15F88A55-4680-11D1-A3B4-00C04FB950DC}';
  IID_IADsFaxNumber : TGUID = '{A910DEA9-4680-11D1-A3B4-00C04FB950DC}';
  CLASS_FaxNumber : TGUID = '{A5062215-4681-11D1-A3B4-00C04FB950DC}';
  IID_IADsNetAddress : TGUID = '{B21A50A9-4080-11D1-A3AC-00C04FB950DC}';
  CLASS_NetAddress : TGUID = '{B0B71247-4080-11D1-A3AC-00C04FB950DC}';
  IID_IADsOctetList : TGUID = '{7B28B80F-4680-11D1-A3B4-00C04FB950DC}';
  CLASS_OctetList : TGUID = '{1241400F-4680-11D1-A3B4-00C04FB950DC}';
  IID_IADsEmail : TGUID = '{97AF011A-478E-11D1-A3B4-00C04FB950DC}';
  CLASS_Email : TGUID = '{8F92A857-478E-11D1-A3B4-00C04FB950DC}';
  IID_IADsPath : TGUID = '{B287FCD5-4080-11D1-A3AC-00C04FB950DC}';
  CLASS_Path : TGUID = '{B2538919-4080-11D1-A3AC-00C04FB950DC}';
  IID_IADsReplicaPointer : TGUID = '{F60FB803-4080-11D1-A3AC-00C04FB950DC}';
  CLASS_ReplicaPointer : TGUID = '{F5D1BADF-4080-11D1-A3AC-00C04FB950DC}';
  IID_IADsAcl : TGUID = '{8452D3AB-0869-11D1-A377-00C04FB950DC}';
  IID_IADsTimestamp : TGUID = '{B2F5A901-4080-11D1-A3AC-00C04FB950DC}';
  CLASS_Timestamp : TGUID = '{B2BED2EB-4080-11D1-A3AC-00C04FB950DC}';
  IID_IADsPostalAddress : TGUID = '{7ADECF29-4680-11D1-A3B4-00C04FB950DC}';
  CLASS_PostalAddress : TGUID = '{0A75AFCD-4680-11D1-A3B4-00C04FB950DC}';
  IID_IADsBackLink : TGUID = '{FD1302BD-4080-11D1-A3AC-00C04FB950DC}';
  CLASS_BackLink : TGUID = '{FCBF906F-4080-11D1-A3AC-00C04FB950DC}';
  IID_IADsTypedName : TGUID = '{B371A349-4080-11D1-A3AC-00C04FB950DC}';
  CLASS_TypedName : TGUID = '{B33143CB-4080-11D1-A3AC-00C04FB950DC}';
  IID_IADsHold : TGUID = '{B3EB3B37-4080-11D1-A3AC-00C04FB950DC}';
  CLASS_Hold : TGUID = '{B3AD3E13-4080-11D1-A3AC-00C04FB950DC}';
  IID_IADsObjectOptions : TGUID = '{46F14FDA-232B-11D1-A808-00C04FD8D5A8}';
  IID_IADsPathname : TGUID = '{D592AED4-F420-11D0-A36E-00C04FB950DC}';
  CLASS_Pathname : TGUID = '{080D0D78-F421-11D0-A36E-00C04FB950DC}';
  IID_IADsADSystemInfo : TGUID = '{5BB11929-AFD1-11D2-9CB9-0000F87A369E}';
  CLASS_ADSystemInfo : TGUID = '{50B6327F-AFD1-11D2-9CB9-0000F87A369E}';
  IID_IADsWinNTSystemInfo : TGUID = '{6C6D65DC-AFD1-11D2-9CB9-0000F87A369E}';
  CLASS_WinNTSystemInfo : TGUID = '{66182EC4-AFD1-11D2-9CB9-0000F87A369E}';
  IID_IADsDNWithBinary : TGUID = '{7E99C0A2-F935-11D2-BA96-00C04FB6D0D1}';
  CLASS_DNWithBinary : TGUID = '{7E99C0A3-F935-11D2-BA96-00C04FB6D0D1}';
  IID_IADsDNWithString : TGUID = '{370DF02E-F934-11D2-BA96-00C04FB6D0D1}';
  CLASS_DNWithString : TGUID = '{334857CC-F934-11D2-BA96-00C04FB6D0D1}';
  IID_IADsSecurityUtility : TGUID = '{A63251B2-5F21-474B-AB52-4A8EFAD10895}';
  CLASS_ADsSecurityUtility : TGUID = '{F270C64A-FFB8-4AE4-85FE-3A75E5347966}';

//Enums

Type
  __MIDL___MIDL_itf_ads_0000_0000_0001 =LongWord;
Const
  ADSTYPE_INVALID = $0000000000000000;
  ADSTYPE_DN_STRING = $0000000000000001;
  ADSTYPE_CASE_EXACT_STRING = $0000000000000002;
  ADSTYPE_CASE_IGNORE_STRING = $0000000000000003;
  ADSTYPE_PRINTABLE_STRING = $0000000000000004;
  ADSTYPE_NUMERIC_STRING = $0000000000000005;
  ADSTYPE_BOOLEAN = $0000000000000006;
  ADSTYPE_INTEGER = $0000000000000007;
  ADSTYPE_OCTET_STRING = $0000000000000008;
  ADSTYPE_UTC_TIME = $0000000000000009;
  ADSTYPE_LARGE_INTEGER = $000000000000000A;
  ADSTYPE_PROV_SPECIFIC = $000000000000000B;
  ADSTYPE_OBJECT_CLASS = $000000000000000C;
  ADSTYPE_CASEIGNORE_LIST = $000000000000000D;
  ADSTYPE_OCTET_LIST = $000000000000000E;
  ADSTYPE_PATH = $000000000000000F;
  ADSTYPE_POSTALADDRESS = $0000000000000010;
  ADSTYPE_TIMESTAMP = $0000000000000011;
  ADSTYPE_BACKLINK = $0000000000000012;
  ADSTYPE_TYPEDNAME = $0000000000000013;
  ADSTYPE_HOLD = $0000000000000014;
  ADSTYPE_NETADDRESS = $0000000000000015;
  ADSTYPE_REPLICAPOINTER = $0000000000000016;
  ADSTYPE_FAXNUMBER = $0000000000000017;
  ADSTYPE_EMAIL = $0000000000000018;
  ADSTYPE_NT_SECURITY_DESCRIPTOR = $0000000000000019;
  ADSTYPE_UNKNOWN = $000000000000001A;
  ADSTYPE_DN_WITH_BINARY = $000000000000001B;
  ADSTYPE_DN_WITH_STRING = $000000000000001C;
Type
  __MIDL___MIDL_itf_ads_0000_0000_0018 =LongWord;
Const
  ADS_SECURE_AUTHENTICATION = $0000000000000001;
  ADS_USE_ENCRYPTION = $0000000000000002;
  ADS_USE_SSL = $0000000000000002;
  ADS_READONLY_SERVER = $0000000000000004;
  ADS_PROMPT_CREDENTIALS = $0000000000000008;
  ADS_NO_AUTHENTICATION = $0000000000000010;
  ADS_FAST_BIND = $0000000000000020;
  ADS_USE_SIGNING = $0000000000000040;
  ADS_USE_SEALING = $0000000000000080;
  ADS_USE_DELEGATION = $0000000000000100;
  ADS_SERVER_BIND = $0000000000000200;
  ADS_NO_REFERRAL_CHASING = $0000000000000400;
  ADS_AUTH_RESERVED = $0000000080000000;
Type
  __MIDL___MIDL_itf_ads_0000_0000_0019 =LongWord;
Const
  ADS_STATUS_S_OK = $0000000000000000;
  ADS_STATUS_INVALID_SEARCHPREF = $0000000000000001;
  ADS_STATUS_INVALID_SEARCHPREFVALUE = $0000000000000002;
Type
  __MIDL___MIDL_itf_ads_0000_0000_0020 =LongWord;
Const
  ADS_DEREF_NEVER = $0000000000000000;
  ADS_DEREF_SEARCHING = $0000000000000001;
  ADS_DEREF_FINDING = $0000000000000002;
  ADS_DEREF_ALWAYS = $0000000000000003;
Type
  __MIDL___MIDL_itf_ads_0000_0000_0021 =LongWord;
Const
  ADS_SCOPE_BASE = $0000000000000000;
  ADS_SCOPE_ONELEVEL = $0000000000000001;
  ADS_SCOPE_SUBTREE = $0000000000000002;
Type
  __MIDL___MIDL_itf_ads_0000_0000_0022 =LongWord;
Const
  ADSIPROP_ASYNCHRONOUS = $0000000000000000;
  ADSIPROP_DEREF_ALIASES = $0000000000000001;
  ADSIPROP_SIZE_LIMIT = $0000000000000002;
  ADSIPROP_TIME_LIMIT = $0000000000000003;
  ADSIPROP_ATTRIBTYPES_ONLY = $0000000000000004;
  ADSIPROP_SEARCH_SCOPE = $0000000000000005;
  ADSIPROP_TIMEOUT = $0000000000000006;
  ADSIPROP_PAGESIZE = $0000000000000007;
  ADSIPROP_PAGED_TIME_LIMIT = $0000000000000008;
  ADSIPROP_CHASE_REFERRALS = $0000000000000009;
  ADSIPROP_SORT_ON = $000000000000000A;
  ADSIPROP_CACHE_RESULTS = $000000000000000B;
  ADSIPROP_ADSIFLAG = $000000000000000C;
Type
  __MIDL___MIDL_itf_ads_0000_0000_0023 =LongWord;
Const
  ADSI_DIALECT_LDAP = $0000000000000000;
  ADSI_DIALECT_SQL = $0000000000000001;
Type
  __MIDL___MIDL_itf_ads_0000_0000_0024 =LongWord;
Const
  ADS_CHASE_REFERRALS_NEVER = $0000000000000000;
  ADS_CHASE_REFERRALS_SUBORDINATE = $0000000000000020;
  ADS_CHASE_REFERRALS_EXTERNAL = $0000000000000040;
  ADS_CHASE_REFERRALS_ALWAYS = $0000000000000060;
Type
  __MIDL___MIDL_itf_ads_0000_0000_0025 =LongWord;
Const
  ADS_SEARCHPREF_ASYNCHRONOUS = $0000000000000000;
  ADS_SEARCHPREF_DEREF_ALIASES = $0000000000000001;
  ADS_SEARCHPREF_SIZE_LIMIT = $0000000000000002;
  ADS_SEARCHPREF_TIME_LIMIT = $0000000000000003;
  ADS_SEARCHPREF_ATTRIBTYPES_ONLY = $0000000000000004;
  ADS_SEARCHPREF_SEARCH_SCOPE = $0000000000000005;
  ADS_SEARCHPREF_TIMEOUT = $0000000000000006;
  ADS_SEARCHPREF_PAGESIZE = $0000000000000007;
  ADS_SEARCHPREF_PAGED_TIME_LIMIT = $0000000000000008;
  ADS_SEARCHPREF_CHASE_REFERRALS = $0000000000000009;
  ADS_SEARCHPREF_SORT_ON = $000000000000000A;
  ADS_SEARCHPREF_CACHE_RESULTS = $000000000000000B;
  ADS_SEARCHPREF_DIRSYNC = $000000000000000C;
  ADS_SEARCHPREF_TOMBSTONE = $000000000000000D;
  ADS_SEARCHPREF_VLV = $000000000000000E;
  ADS_SEARCHPREF_ATTRIBUTE_QUERY = $000000000000000F;
  ADS_SEARCHPREF_SECURITY_MASK = $0000000000000010;
  ADS_SEARCHPREF_DIRSYNC_FLAG = $0000000000000011;
  ADS_SEARCHPREF_EXTENDED_DN = $0000000000000012;
Type
  __MIDL___MIDL_itf_ads_0000_0000_0026 =LongWord;
Const
  ADS_PASSWORD_ENCODE_REQUIRE_SSL = $0000000000000000;
  ADS_PASSWORD_ENCODE_CLEAR = $0000000000000001;
Type
  __MIDL___MIDL_itf_ads_0000_0000_0027 =LongWord;
Const
  ADS_PROPERTY_CLEAR = $0000000000000001;
  ADS_PROPERTY_UPDATE = $0000000000000002;
  ADS_PROPERTY_APPEND = $0000000000000003;
  ADS_PROPERTY_DELETE = $0000000000000004;
Type
  tagTYPEKIND =LongWord;
Const
  TKIND_ENUM = $0000000000000000;
  TKIND_RECORD = $0000000000000001;
  TKIND_MODULE = $0000000000000002;
  TKIND_INTERFACE = $0000000000000003;
  TKIND_DISPATCH = $0000000000000004;
  TKIND_COCLASS = $0000000000000005;
  TKIND_ALIAS = $0000000000000006;
  TKIND_UNION = $0000000000000007;
  TKIND_MAX = $0000000000000008;
Type
  tagDESCKIND =LongWord;
Const
  DESCKIND_NONE = $0000000000000000;
  DESCKIND_FUNCDESC = $0000000000000001;
  DESCKIND_VARDESC = $0000000000000002;
  DESCKIND_TYPECOMP = $0000000000000003;
  DESCKIND_IMPLICITAPPOBJ = $0000000000000004;
  DESCKIND_MAX = $0000000000000005;
Type
  tagFUNCKIND =LongWord;
Const
  FUNC_VIRTUAL = $0000000000000000;
  FUNC_PUREVIRTUAL = $0000000000000001;
  FUNC_NONVIRTUAL = $0000000000000002;
  FUNC_STATIC = $0000000000000003;
  FUNC_DISPATCH = $0000000000000004;
Type
  tagINVOKEKIND =LongWord;
Const
  INVOKE_FUNC = $0000000000000001;
  INVOKE_PROPERTYGET = $0000000000000002;
  INVOKE_PROPERTYPUT = $0000000000000004;
  INVOKE_PROPERTYPUTREF = $0000000000000008;
Type
  tagCALLCONV =LongWord;
Const
  CC_FASTCALL = $0000000000000000;
  CC_CDECL = $0000000000000001;
  CC_MSCPASCAL = $0000000000000002;
  CC_PASCAL = $0000000000000002;
  CC_MACPASCAL = $0000000000000003;
  CC_STDCALL = $0000000000000004;
  CC_FPFASTCALL = $0000000000000005;
  CC_SYSCALL = $0000000000000006;
  CC_MPWCDECL = $0000000000000007;
  CC_MPWPASCAL = $0000000000000008;
  CC_MAX = $0000000000000009;
Type
  tagVARKIND =LongWord;
Const
  VAR_PERINSTANCE = $0000000000000000;
  VAR_STATIC = $0000000000000001;
  VAR_CONST = $0000000000000002;
  VAR_DISPATCH = $0000000000000003;
Type
  tagSYSKIND =LongWord;
Const
  SYS_WIN16 = $0000000000000000;
  SYS_WIN32 = $0000000000000001;
  SYS_MAC = $0000000000000002;
  SYS_WIN64 = $0000000000000003;
Type
  __MIDL___MIDL_itf_ads_0001_0015_0001 =LongWord;
Const
  ADS_SYSTEMFLAG_DISALLOW_DELETE = $0000000080000000;
  ADS_SYSTEMFLAG_CONFIG_ALLOW_RENAME = $0000000040000000;
  ADS_SYSTEMFLAG_CONFIG_ALLOW_MOVE = $0000000020000000;
  ADS_SYSTEMFLAG_CONFIG_ALLOW_LIMITED_MOVE = $0000000010000000;
  ADS_SYSTEMFLAG_DOMAIN_DISALLOW_RENAME = $0000000008000000;
  ADS_SYSTEMFLAG_DOMAIN_DISALLOW_MOVE = $0000000004000000;
  ADS_SYSTEMFLAG_CR_NTDS_NC = $0000000000000001;
  ADS_SYSTEMFLAG_CR_NTDS_DOMAIN = $0000000000000002;
  ADS_SYSTEMFLAG_ATTR_NOT_REPLICATED = $0000000000000001;
  ADS_SYSTEMFLAG_ATTR_IS_CONSTRUCTED = $0000000000000004;
Type
  __MIDL___MIDL_itf_ads_0001_0021_0001 =LongWord;
Const
  ADS_GROUP_TYPE_GLOBAL_GROUP = $0000000000000002;
  ADS_GROUP_TYPE_DOMAIN_LOCAL_GROUP = $0000000000000004;
  ADS_GROUP_TYPE_LOCAL_GROUP = $0000000000000004;
  ADS_GROUP_TYPE_UNIVERSAL_GROUP = $0000000000000008;
  ADS_GROUP_TYPE_SECURITY_ENABLED = $0000000080000000;
Type
  ADS_USER_FLAG =LongWord;
Const
  ADS_UF_SCRIPT = $0000000000000001;
  ADS_UF_ACCOUNTDISABLE = $0000000000000002;
  ADS_UF_HOMEDIR_REQUIRED = $0000000000000008;
  ADS_UF_LOCKOUT = $0000000000000010;
  ADS_UF_PASSWD_NOTREQD = $0000000000000020;
  ADS_UF_PASSWD_CANT_CHANGE = $0000000000000040;
  ADS_UF_ENCRYPTED_TEXT_PASSWORD_ALLOWED = $0000000000000080;
  ADS_UF_TEMP_DUPLICATE_ACCOUNT = $0000000000000100;
  ADS_UF_NORMAL_ACCOUNT = $0000000000000200;
  ADS_UF_INTERDOMAIN_TRUST_ACCOUNT = $0000000000000800;
  ADS_UF_WORKSTATION_TRUST_ACCOUNT = $0000000000001000;
  ADS_UF_SERVER_TRUST_ACCOUNT = $0000000000002000;
  ADS_UF_DONT_EXPIRE_PASSWD = $0000000000010000;
  ADS_UF_MNS_LOGON_ACCOUNT = $0000000000020000;
  ADS_UF_SMARTCARD_REQUIRED = $0000000000040000;
  ADS_UF_TRUSTED_FOR_DELEGATION = $0000000000080000;
  ADS_UF_NOT_DELEGATED = $0000000000100000;
  ADS_UF_USE_DES_KEY_ONLY = $0000000000200000;
  ADS_UF_DONT_REQUIRE_PREAUTH = $0000000000400000;
  ADS_UF_PASSWORD_EXPIRED = $0000000000800000;
  ADS_UF_TRUSTED_TO_AUTHENTICATE_FOR_DELEGATION = $0000000001000000;
Type
  __MIDL___MIDL_itf_ads_0001_0043_0001 =LongWord;
Const
  ADS_RIGHT_DELETE = $0000000000010000;
  ADS_RIGHT_READ_CONTROL = $0000000000020000;
  ADS_RIGHT_WRITE_DAC = $0000000000040000;
  ADS_RIGHT_WRITE_OWNER = $0000000000080000;
  ADS_RIGHT_SYNCHRONIZE = $0000000000100000;
  ADS_RIGHT_ACCESS_SYSTEM_SECURITY = $0000000001000000;
  ADS_RIGHT_GENERIC_READ = $0000000080000000;
  ADS_RIGHT_GENERIC_WRITE = $0000000040000000;
  ADS_RIGHT_GENERIC_EXECUTE = $0000000020000000;
  ADS_RIGHT_GENERIC_ALL = $0000000010000000;
  ADS_RIGHT_DS_CREATE_CHILD = $0000000000000001;
  ADS_RIGHT_DS_DELETE_CHILD = $0000000000000002;
  ADS_RIGHT_ACTRL_DS_LIST = $0000000000000004;
  ADS_RIGHT_DS_SELF = $0000000000000008;
  ADS_RIGHT_DS_READ_PROP = $0000000000000010;
  ADS_RIGHT_DS_WRITE_PROP = $0000000000000020;
  ADS_RIGHT_DS_DELETE_TREE = $0000000000000040;
  ADS_RIGHT_DS_LIST_OBJECT = $0000000000000080;
  ADS_RIGHT_DS_CONTROL_ACCESS = $0000000000000100;
Type
  __MIDL___MIDL_itf_ads_0001_0043_0002 =LongWord;
Const
  ADS_ACETYPE_ACCESS_ALLOWED = $0000000000000000;
  ADS_ACETYPE_ACCESS_DENIED = $0000000000000001;
  ADS_ACETYPE_SYSTEM_AUDIT = $0000000000000002;
  ADS_ACETYPE_ACCESS_ALLOWED_OBJECT = $0000000000000005;
  ADS_ACETYPE_ACCESS_DENIED_OBJECT = $0000000000000006;
  ADS_ACETYPE_SYSTEM_AUDIT_OBJECT = $0000000000000007;
  ADS_ACETYPE_SYSTEM_ALARM_OBJECT = $0000000000000008;
  ADS_ACETYPE_ACCESS_ALLOWED_CALLBACK = $0000000000000009;
  ADS_ACETYPE_ACCESS_DENIED_CALLBACK = $000000000000000A;
  ADS_ACETYPE_ACCESS_ALLOWED_CALLBACK_OBJECT = $000000000000000B;
  ADS_ACETYPE_ACCESS_DENIED_CALLBACK_OBJECT = $000000000000000C;
  ADS_ACETYPE_SYSTEM_AUDIT_CALLBACK = $000000000000000D;
  ADS_ACETYPE_SYSTEM_ALARM_CALLBACK = $000000000000000E;
  ADS_ACETYPE_SYSTEM_AUDIT_CALLBACK_OBJECT = $000000000000000F;
  ADS_ACETYPE_SYSTEM_ALARM_CALLBACK_OBJECT = $0000000000000010;
Type
  __MIDL___MIDL_itf_ads_0001_0043_0003 =LongWord;
Const
  ADS_ACEFLAG_INHERIT_ACE = $0000000000000002;
  ADS_ACEFLAG_NO_PROPAGATE_INHERIT_ACE = $0000000000000004;
  ADS_ACEFLAG_INHERIT_ONLY_ACE = $0000000000000008;
  ADS_ACEFLAG_INHERITED_ACE = $0000000000000010;
  ADS_ACEFLAG_VALID_INHERIT_FLAGS = $000000000000001F;
  ADS_ACEFLAG_SUCCESSFUL_ACCESS = $0000000000000040;
  ADS_ACEFLAG_FAILED_ACCESS = $0000000000000080;
Type
  __MIDL___MIDL_itf_ads_0001_0043_0004 =LongWord;
Const
  ADS_FLAG_OBJECT_TYPE_PRESENT = $0000000000000001;
  ADS_FLAG_INHERITED_OBJECT_TYPE_PRESENT = $0000000000000002;
Type
  __MIDL___MIDL_itf_ads_0001_0043_0005 =LongWord;
Const
  ADS_SD_CONTROL_SE_OWNER_DEFAULTED = $0000000000000001;
  ADS_SD_CONTROL_SE_GROUP_DEFAULTED = $0000000000000002;
  ADS_SD_CONTROL_SE_DACL_PRESENT = $0000000000000004;
  ADS_SD_CONTROL_SE_DACL_DEFAULTED = $0000000000000008;
  ADS_SD_CONTROL_SE_SACL_PRESENT = $0000000000000010;
  ADS_SD_CONTROL_SE_SACL_DEFAULTED = $0000000000000020;
  ADS_SD_CONTROL_SE_DACL_AUTO_INHERIT_REQ = $0000000000000100;
  ADS_SD_CONTROL_SE_SACL_AUTO_INHERIT_REQ = $0000000000000200;
  ADS_SD_CONTROL_SE_DACL_AUTO_INHERITED = $0000000000000400;
  ADS_SD_CONTROL_SE_SACL_AUTO_INHERITED = $0000000000000800;
  ADS_SD_CONTROL_SE_DACL_PROTECTED = $0000000000001000;
  ADS_SD_CONTROL_SE_SACL_PROTECTED = $0000000000002000;
  ADS_SD_CONTROL_SE_SELF_RELATIVE = $0000000000008000;
Type
  __MIDL___MIDL_itf_ads_0001_0043_0006 =LongWord;
Const
  ADS_SD_REVISION_DS = $0000000000000004;
Type
  __MIDL___MIDL_itf_ads_0001_0044_0001 =LongWord;
Const
  ADS_NAME_TYPE_1779 = $0000000000000001;
  ADS_NAME_TYPE_CANONICAL = $0000000000000002;
  ADS_NAME_TYPE_NT4 = $0000000000000003;
  ADS_NAME_TYPE_DISPLAY = $0000000000000004;
  ADS_NAME_TYPE_DOMAIN_SIMPLE = $0000000000000005;
  ADS_NAME_TYPE_ENTERPRISE_SIMPLE = $0000000000000006;
  ADS_NAME_TYPE_GUID = $0000000000000007;
  ADS_NAME_TYPE_UNKNOWN = $0000000000000008;
  ADS_NAME_TYPE_USER_PRINCIPAL_NAME = $0000000000000009;
  ADS_NAME_TYPE_CANONICAL_EX = $000000000000000A;
  ADS_NAME_TYPE_SERVICE_PRINCIPAL_NAME = $000000000000000B;
  ADS_NAME_TYPE_SID_OR_SID_HISTORY_NAME = $000000000000000C;
Type
  __MIDL___MIDL_itf_ads_0001_0044_0002 =LongWord;
Const
  ADS_NAME_INITTYPE_DOMAIN = $0000000000000001;
  ADS_NAME_INITTYPE_SERVER = $0000000000000002;
  ADS_NAME_INITTYPE_GC = $0000000000000003;
Type
  __MIDL___MIDL_itf_ads_0001_0058_0001 =LongWord;
Const
  ADS_OPTION_SERVERNAME = $0000000000000000;
  ADS_OPTION_REFERRALS = $0000000000000001;
  ADS_OPTION_PAGE_SIZE = $0000000000000002;
  ADS_OPTION_SECURITY_MASK = $0000000000000003;
  ADS_OPTION_MUTUAL_AUTH_STATUS = $0000000000000004;
  ADS_OPTION_QUOTA = $0000000000000005;
  ADS_OPTION_PASSWORD_PORTNUMBER = $0000000000000006;
  ADS_OPTION_PASSWORD_METHOD = $0000000000000007;
  ADS_OPTION_ACCUMULATIVE_MODIFICATION = $0000000000000008;
  ADS_OPTION_SKIP_SID_LOOKUP = $0000000000000009;
Type
  __MIDL___MIDL_itf_ads_0001_0058_0002 =LongWord;
Const
  ADS_SECURITY_INFO_OWNER = $0000000000000001;
  ADS_SECURITY_INFO_GROUP = $0000000000000002;
  ADS_SECURITY_INFO_DACL = $0000000000000004;
  ADS_SECURITY_INFO_SACL = $0000000000000008;
Type
  __MIDL___MIDL_itf_ads_0001_0059_0001 =LongWord;
Const
  ADS_SETTYPE_FULL = $0000000000000001;
  ADS_SETTYPE_PROVIDER = $0000000000000002;
  ADS_SETTYPE_SERVER = $0000000000000003;
  ADS_SETTYPE_DN = $0000000000000004;
Type
  __MIDL___MIDL_itf_ads_0001_0059_0002 =LongWord;
Const
  ADS_FORMAT_WINDOWS = $0000000000000001;
  ADS_FORMAT_WINDOWS_NO_SERVER = $0000000000000002;
  ADS_FORMAT_WINDOWS_DN = $0000000000000003;
  ADS_FORMAT_WINDOWS_PARENT = $0000000000000004;
  ADS_FORMAT_X500 = $0000000000000005;
  ADS_FORMAT_X500_NO_SERVER = $0000000000000006;
  ADS_FORMAT_X500_DN = $0000000000000007;
  ADS_FORMAT_X500_PARENT = $0000000000000008;
  ADS_FORMAT_SERVER = $0000000000000009;
  ADS_FORMAT_PROVIDER = $000000000000000A;
  ADS_FORMAT_LEAF = $000000000000000B;
Type
  __MIDL___MIDL_itf_ads_0001_0059_0003 =LongWord;
Const
  ADS_DISPLAY_FULL = $0000000000000001;
  ADS_DISPLAY_VALUE_ONLY = $0000000000000002;
Type
  __MIDL___MIDL_itf_ads_0001_0059_0004 =LongWord;
Const
  ADS_ESCAPEDMODE_DEFAULT = $0000000000000001;
  ADS_ESCAPEDMODE_ON = $0000000000000002;
  ADS_ESCAPEDMODE_OFF = $0000000000000003;
  ADS_ESCAPEDMODE_OFF_EX = $0000000000000004;
Type
  __MIDL___MIDL_itf_ads_0001_0064_0001 =LongWord;
Const
  ADS_PATH_FILE = $0000000000000001;
  ADS_PATH_FILESHARE = $0000000000000002;
  ADS_PATH_REGISTRY = $0000000000000003;
Type
  __MIDL___MIDL_itf_ads_0001_0064_0002 =LongWord;
Const
  ADS_SD_FORMAT_IID = $0000000000000001;
  ADS_SD_FORMAT_RAW = $0000000000000002;
  ADS_SD_FORMAT_HEXSTRING = $0000000000000003;
//Forward declarations

Type
 IADs = interface;
 IADsDisp = dispinterface;
 IADsContainer = interface;
 IADsContainerDisp = dispinterface;
 IADsCollection = interface;
 IADsCollectionDisp = dispinterface;
 IADsMembers = interface;
 IADsMembersDisp = dispinterface;
 IADsPropertyList = interface;
 IADsPropertyListDisp = dispinterface;
 IADsPropertyEntry = interface;
 IADsPropertyEntryDisp = dispinterface;
 IADsPropertyValue = interface;
 IADsPropertyValueDisp = dispinterface;
 IADsPropertyValue2 = interface;
 IADsPropertyValue2Disp = dispinterface;
 IPrivateDispatch = interface;
 ITypeInfo = interface;
 ITypeComp = interface;
 ITypeLib = interface;
 IPrivateUnknown = interface;
 IADsExtension = interface;
 IADsDeleteOps = interface;
 IADsDeleteOpsDisp = dispinterface;
 IADsNamespaces = interface;
 IADsNamespacesDisp = dispinterface;
 IADsClass = interface;
 IADsClassDisp = dispinterface;
 IADsProperty = interface;
 IADsPropertyDisp = dispinterface;
 IADsSyntax = interface;
 IADsSyntaxDisp = dispinterface;
 IADsLocality = interface;
 IADsLocalityDisp = dispinterface;
 IADsO = interface;
 IADsODisp = dispinterface;
 IADsOU = interface;
 IADsOUDisp = dispinterface;
 IADsDomain = interface;
 IADsDomainDisp = dispinterface;
 IADsComputer = interface;
 IADsComputerDisp = dispinterface;
 IADsComputerOperations = interface;
 IADsComputerOperationsDisp = dispinterface;
 IADsGroup = interface;
 IADsGroupDisp = dispinterface;
 IADsUser = interface;
 IADsUserDisp = dispinterface;
 IADsPrintQueue = interface;
 IADsPrintQueueDisp = dispinterface;
 IADsPrintQueueOperations = interface;
 IADsPrintQueueOperationsDisp = dispinterface;
 IADsPrintJob = interface;
 IADsPrintJobDisp = dispinterface;
 IADsPrintJobOperations = interface;
 IADsPrintJobOperationsDisp = dispinterface;
 IADsService = interface;
 IADsServiceDisp = dispinterface;
 IADsServiceOperations = interface;
 IADsServiceOperationsDisp = dispinterface;
 IADsFileService = interface;
 IADsFileServiceDisp = dispinterface;
 IADsFileServiceOperations = interface;
 IADsFileServiceOperationsDisp = dispinterface;
 IADsFileShare = interface;
 IADsFileShareDisp = dispinterface;
 IADsSession = interface;
 IADsSessionDisp = dispinterface;
 IADsResource = interface;
 IADsResourceDisp = dispinterface;
 IADsOpenDSObject = interface;
 IADsOpenDSObjectDisp = dispinterface;
 IDirectoryObject = interface;
 IDirectorySearch = interface;
 IDirectorySchemaMgmt = interface;
 IADsAggregatee = interface;
 IADsAggregator = interface;
 IADsAccessControlEntry = interface;
 IADsAccessControlEntryDisp = dispinterface;
 IADsAccessControlList = interface;
 IADsAccessControlListDisp = dispinterface;
 IADsSecurityDescriptor = interface;
 IADsSecurityDescriptorDisp = dispinterface;
 IADsLargeInteger = interface;
 IADsLargeIntegerDisp = dispinterface;
 IADsNameTranslate = interface;
 IADsNameTranslateDisp = dispinterface;
 IADsCaseIgnoreList = interface;
 IADsCaseIgnoreListDisp = dispinterface;
 IADsFaxNumber = interface;
 IADsFaxNumberDisp = dispinterface;
 IADsNetAddress = interface;
 IADsNetAddressDisp = dispinterface;
 IADsOctetList = interface;
 IADsOctetListDisp = dispinterface;
 IADsEmail = interface;
 IADsEmailDisp = dispinterface;
 IADsPath = interface;
 IADsPathDisp = dispinterface;
 IADsReplicaPointer = interface;
 IADsReplicaPointerDisp = dispinterface;
 IADsAcl = interface;
 IADsAclDisp = dispinterface;
 IADsTimestamp = interface;
 IADsTimestampDisp = dispinterface;
 IADsPostalAddress = interface;
 IADsPostalAddressDisp = dispinterface;
 IADsBackLink = interface;
 IADsBackLinkDisp = dispinterface;
 IADsTypedName = interface;
 IADsTypedNameDisp = dispinterface;
 IADsHold = interface;
 IADsHoldDisp = dispinterface;
 IADsObjectOptions = interface;
 IADsObjectOptionsDisp = dispinterface;
 IADsPathname = interface;
 IADsPathnameDisp = dispinterface;
 IADsADSystemInfo = interface;
 IADsADSystemInfoDisp = dispinterface;
 IADsWinNTSystemInfo = interface;
 IADsWinNTSystemInfoDisp = dispinterface;
 IADsDNWithBinary = interface;
 IADsDNWithBinaryDisp = dispinterface;
 IADsDNWithString = interface;
 IADsDNWithStringDisp = dispinterface;
 IADsSecurityUtility = interface;
 IADsSecurityUtilityDisp = dispinterface;

//Map CoClass to its default interface

 PropertyEntry = IADsPropertyEntry;
 PropertyValue = IADsPropertyValue;
 AccessControlEntry = IADsAccessControlEntry;
 AccessControlList = IADsAccessControlList;
 SecurityDescriptor = IADsSecurityDescriptor;
 LargeInteger = IADsLargeInteger;
 NameTranslate = IADsNameTranslate;
 CaseIgnoreList = IADsCaseIgnoreList;
 FaxNumber = IADsFaxNumber;
 NetAddress = IADsNetAddress;
 OctetList = IADsOctetList;
 Email = IADsEmail;
 Path = IADsPath;
 ReplicaPointer = IADsReplicaPointer;
 Timestamp = IADsTimestamp;
 PostalAddress = IADsPostalAddress;
 BackLink = IADsBackLink;
 TypedName = IADsTypedName;
 Hold = IADsHold;
 Pathname = IADsPathname;
 ADSystemInfo = IADsADSystemInfo;
 WinNTSystemInfo = IADsWinNTSystemInfo;
 DNWithBinary = IADsDNWithBinary;
 DNWithString = IADsDNWithString;
 ADsSecurityUtility = IADsSecurityUtility;

//records, unions, aliases

 ADSTYPEENUM = __MIDL___MIDL_itf_ads_0000_0000_0001;
 P__MIDL___MIDL_itf_ads_0000_0000_0002 = ^__MIDL___MIDL_itf_ads_0000_0000_0002;

 __MIDL___MIDL_itf_ads_0000_0000_0002 = packed record
     dwLength : LongWord;
     lpValue : PByte;
 end;
 ADS_OCTET_STRING = TGUID;//__MIDL___MIDL_itf_ads_0000_0000_0002;
 P__MIDL___MIDL_itf_ads_0000_0000_0003 = ^__MIDL___MIDL_itf_ads_0000_0000_0003;

 __MIDL___MIDL_itf_ads_0000_0000_0003 = packed record
     dwLength : LongWord;
     lpValue : PByte;
 end;
 ADS_NT_SECURITY_DESCRIPTOR = TGUID;//__MIDL___MIDL_itf_ads_0000_0000_0003;
 P_SYSTEMTIME = ^_SYSTEMTIME;

 _SYSTEMTIME = TSystemTime;{packed record
     wYear : Word;
     wMonth : Word;
     wDayOfWeek : Word;
     wDay : Word;
     wHour : Word;
     wMinute : Word;
     wSecond : Word;
     wMilliseconds : Word;
 end;}
 P_LARGE_INTEGER = ^_LARGE_INTEGER;

 _LARGE_INTEGER = Int64;{packed record
     QuadPart : Int64;
 end;}
 P__MIDL___MIDL_itf_ads_0000_0000_0004 = ^__MIDL___MIDL_itf_ads_0000_0000_0004;

 __MIDL___MIDL_itf_ads_0000_0000_0004 = packed record
     dwLength : LongWord;
     lpValue : PByte;
 end;
 ADS_PROV_SPECIFIC = TGUID;//__MIDL___MIDL_itf_ads_0000_0000_0004;
 P_ADS_CASEIGNORE_LIST = ^_ADS_CASEIGNORE_LIST;

 _ADS_CASEIGNORE_LIST = packed record
     Next : P_ADS_CASEIGNORE_LIST;
     String_ : PWideChar;
 end;
 P_ADS_OCTET_LIST = ^_ADS_OCTET_LIST;

 _ADS_OCTET_LIST = packed record
     Next : P_ADS_OCTET_LIST;
     Length : LongWord;
     Data : PByte;
 end;
 P__MIDL___MIDL_itf_ads_0000_0000_0005 = ^TGUID;//__MIDL___MIDL_itf_ads_0000_0000_0005;

 __MIDL___MIDL_itf_ads_0000_0000_0005 = packed record
     Type_ : LongWord;
     VolumeName : PWideChar;
     Path : PWideChar;
 end;
 ADS_PATH = __MIDL___MIDL_itf_ads_0000_0000_0005;
 P__MIDL___MIDL_itf_ads_0000_0000_0006 = ^TGUID;//__MIDL___MIDL_itf_ads_0000_0000_0006;

 __MIDL___MIDL_itf_ads_0000_0000_0006 = packed record
     PostalAddress : array[0..5] of PWideChar;
 end;
 ADS_POSTALADDRESS = __MIDL___MIDL_itf_ads_0000_0000_0006;
 P__MIDL___MIDL_itf_ads_0000_0000_0007 = ^__MIDL___MIDL_itf_ads_0000_0000_0007;

 __MIDL___MIDL_itf_ads_0000_0000_0007 = packed record
     WholeSeconds : LongWord;
     EventID : LongWord;
 end;
 ADS_TIMESTAMP = TGUID;//__MIDL___MIDL_itf_ads_0000_0000_0007;
 P__MIDL___MIDL_itf_ads_0000_0000_0008 = ^__MIDL___MIDL_itf_ads_0000_0000_0008;

 __MIDL___MIDL_itf_ads_0000_0000_0008 = packed record
     RemoteID : LongWord;
     ObjectName : PWideChar;
 end;
 ADS_BACKLINK = TGUID;//__MIDL___MIDL_itf_ads_0000_0000_0008;
 P__MIDL___MIDL_itf_ads_0000_0000_0009 = ^__MIDL___MIDL_itf_ads_0000_0000_0009;

 __MIDL___MIDL_itf_ads_0000_0000_0009 = packed record
     ObjectName : PWideChar;
     Level : LongWord;
     Interval : LongWord;
 end;
 ADS_TYPEDNAME = __MIDL___MIDL_itf_ads_0000_0000_0009;
 P__MIDL___MIDL_itf_ads_0000_0000_0010 = ^__MIDL___MIDL_itf_ads_0000_0000_0010;

 __MIDL___MIDL_itf_ads_0000_0000_0010 = packed record
     ObjectName : PWideChar;
     Amount : LongWord;
 end;
 ADS_HOLD = TGUID;//__MIDL___MIDL_itf_ads_0000_0000_0010;
 P__MIDL___MIDL_itf_ads_0000_0000_0011 = ^TGUID;//__MIDL___MIDL_itf_ads_0000_0000_0011;

 __MIDL___MIDL_itf_ads_0000_0000_0011 = packed record
     AddressType : LongWord;
     AddressLength : LongWord;
     Address : PByte;
 end;
 ADS_NETADDRESS = __MIDL___MIDL_itf_ads_0000_0000_0011;
 P__MIDL___MIDL_itf_ads_0000_0000_0012 = ^TGUID;//__MIDL___MIDL_itf_ads_0000_0000_0012;

 __MIDL___MIDL_itf_ads_0000_0000_0012 = packed record
     ServerName : PWideChar;
     ReplicaType : LongWord;
     ReplicaNumber : LongWord;
     Count : LongWord;
     ReplicaAddressHints : P__MIDL___MIDL_itf_ads_0000_0000_0011;
 end;
 ADS_REPLICAPOINTER = __MIDL___MIDL_itf_ads_0000_0000_0012;
 P__MIDL___MIDL_itf_ads_0000_0000_0013 = ^TGUID;//__MIDL___MIDL_itf_ads_0000_0000_0013;

 __MIDL___MIDL_itf_ads_0000_0000_0013 = packed record
     TelephoneNumber : PWideChar;
     NumberOfBits : LongWord;
     Parameters : PByte;
 end;
 ADS_FAXNUMBER = __MIDL___MIDL_itf_ads_0000_0000_0013;
 P__MIDL___MIDL_itf_ads_0000_0000_0014 = ^__MIDL___MIDL_itf_ads_0000_0000_0014;

 __MIDL___MIDL_itf_ads_0000_0000_0014 = packed record
     Address : PWideChar;
     Type_ : LongWord;
 end;
 ADS_EMAIL = TGUID;//__MIDL___MIDL_itf_ads_0000_0000_0014;
 P__MIDL___MIDL_itf_ads_0000_0000_0015 = ^TGUID;//__MIDL___MIDL_itf_ads_0000_0000_0015;

 __MIDL___MIDL_itf_ads_0000_0000_0015 = packed record
     dwLength : LongWord;
     lpBinaryValue : PByte;
     pszDNString : PWideChar;
 end;
 ADS_DN_WITH_BINARY = __MIDL___MIDL_itf_ads_0000_0000_0015;
 P__MIDL___MIDL_itf_ads_0000_0000_0016 = ^TGUID;//__MIDL___MIDL_itf_ads_0000_0000_0016;

 __MIDL___MIDL_itf_ads_0000_0000_0016 = packed record
     pszStringValue : PWideChar;
     pszDNString : PWideChar;
 end;
 ADS_DN_WITH_STRING = __MIDL___MIDL_itf_ads_0000_0000_0016;
 P_adsvalue = ^_adsvalue;
 PADSVALUE = ^_ADSVALUE;

 P__MIDL___MIDL_itf_ads_0000_0000_0017 = ^__MIDL___MIDL_itf_ads_0000_0000_0017;

 __MIDL___MIDL_itf_ads_0000_0000_0017 =  record
    case Integer of
     0: (DNString : PWideChar);
     1: (CaseExactString : PWideChar);
     2: (CaseIgnoreString : PWideChar);
     3: (PrintableString : PWideChar);
     4: (NumericString : PWideChar);
     5: (Boolean_ : LongWord);
     6: (Integer_ : LongWord);
     7: (OctetString : ADS_OCTET_STRING);
     8: (UTCTime : _SYSTEMTIME);
     9: (LargeInteger : _LARGE_INTEGER);
     10: (ClassName : PWideChar);
     11: (ProviderSpecific : ADS_PROV_SPECIFIC);
     12: (pCaseIgnoreList : P_ADS_CASEIGNORE_LIST);
     13: (pOctetList : P_ADS_OCTET_LIST);
     14: (pPath : P__MIDL___MIDL_itf_ads_0000_0000_0005);
     15: (pPostalAddress : P__MIDL___MIDL_itf_ads_0000_0000_0006);
     16: (Timestamp : ADS_TIMESTAMP);
     17: (BackLink : ADS_BACKLINK);
     18: (pTypedName : P__MIDL___MIDL_itf_ads_0000_0000_0009);
     19: (Hold : ADS_HOLD);
     20: (pNetAddress : P__MIDL___MIDL_itf_ads_0000_0000_0011);
     21: (pReplicaPointer : P__MIDL___MIDL_itf_ads_0000_0000_0012);
     22: (pFaxNumber : P__MIDL___MIDL_itf_ads_0000_0000_0013);
     23: (Email : ADS_EMAIL);
     24: (SecurityDescriptor : ADS_NT_SECURITY_DESCRIPTOR);
     25: (pDNWithBinary : P__MIDL___MIDL_itf_ads_0000_0000_0015);
     26: (pDNWithString : P__MIDL___MIDL_itf_ads_0000_0000_0016);
 end;
 _adsvalue = packed record
     dwType : ADSTYPEENUM;
     __MIDL_0010 : __MIDL___MIDL_itf_ads_0000_0000_0017;
 end;
 P_ads_attr_info = ^_ads_attr_info;

 _ads_attr_info = packed record
     pszAttrName : PWideChar;
     dwControlCode : LongWord;
     dwADsType : ADSTYPEENUM;
     pADsValues : P_adsvalue;
     dwNumValues : LongWord;
 end;
 ADS_AUTHENTICATION_ENUM = __MIDL___MIDL_itf_ads_0000_0000_0018;
 P_ads_object_info = ^_ads_object_info;

 _ads_object_info = packed record
     pszRDN : PWideChar;
     pszObjectDN : PWideChar;
     pszParentDN : PWideChar;
     pszSchemaDN : PWideChar;
     pszClassName : PWideChar;
 end;
 ADS_STATUSENUM = __MIDL___MIDL_itf_ads_0000_0000_0019;
 ADS_DEREFENUM = __MIDL___MIDL_itf_ads_0000_0000_0020;
 ADS_SCOPEENUM = __MIDL___MIDL_itf_ads_0000_0000_0021;
 ADS_PREFERENCES_ENUM = __MIDL___MIDL_itf_ads_0000_0000_0022;
 ADSI_DIALECT_ENUM = __MIDL___MIDL_itf_ads_0000_0000_0023;
 ADS_CHASE_REFERRALS_ENUM = __MIDL___MIDL_itf_ads_0000_0000_0024;
 ADS_SEARCHPREF_ENUM = __MIDL___MIDL_itf_ads_0000_0000_0025;
 ADS_PASSWORD_ENCODING_ENUM = __MIDL___MIDL_itf_ads_0000_0000_0026;
 Pads_searchpref_info = ^ads_searchpref_info;

 ads_searchpref_info = packed record
     dwSearchPref : ADS_SEARCHPREF_ENUM;
     vValue : _adsvalue;
     dwStatus : ADS_STATUSENUM;
 end;
 Pads_search_column = ^ads_search_column;

 ads_search_column = packed record
     pszAttrName : PWideChar;
     dwADsType : ADSTYPEENUM;
     pADsValues : P_adsvalue;
     dwNumValues : LongWord;
     hReserved : Ppointer;
 end;
 P_ads_attr_def = ^_ads_attr_def;

 _ads_attr_def = packed record
     pszAttrName : PWideChar;
     dwADsType : ADSTYPEENUM;
     dwMinRange : LongWord;
     dwMaxRange : LongWord;
     fMultiValued : Integer;
 end;
 P_ads_class_def = ^_ads_class_def;

 _ads_class_def = packed record
     pszClassName : PWideChar;
     dwMandatoryAttrs : LongWord;
     ppszMandatoryAttrs : PPWideChar;
     optionalAttrs : LongWord;
     ppszOptionalAttrs : PPPWideChar;
     dwNamingAttrs : LongWord;
     ppszNamingAttrs : PPPWideChar;
     dwSuperClasses : LongWord;
     ppszSuperClasses : PPPWideChar;
     fIsContainer : Integer;
 end;
 P_ads_sortkey = ^_ads_sortkey;

 _ads_sortkey = packed record
     pszAttrType : PWideChar;
     pszReserved : PWideChar;
     fReverseorder : ShortInt;
 end;
 P_ads_vlv = ^_ads_vlv;

 _ads_vlv = packed record
     dwBeforeCount : LongWord;
     dwAfterCount : LongWord;
     dwOffset : LongWord;
     dwContentCount : LongWord;
     pszTarget : PWideChar;
     dwContextIDLength : LongWord;
     lpContextID : PByte;
 end;
 ADS_PROPERTY_OPERATION_ENUM = __MIDL___MIDL_itf_ads_0000_0000_0027;
 PtagTYPEATTR = ^tagTYPEATTR;

 PtagTYPEDESC = ^tagTYPEDESC;

 P__MIDL_IOleAutomationTypes_0005 = ^__MIDL_IOleAutomationTypes_0005;

 PtagARRAYDESC = ^tagARRAYDESC;

 __MIDL_IOleAutomationTypes_0005 =  record
    case Integer of
     0: (lptdesc : PtagTYPEDESC);
     1: (lpadesc : PtagARRAYDESC);
     2: (hreftype : LongWord);
 end;
 tagTYPEDESC = packed record
     DUMMYUNIONNAME : __MIDL_IOleAutomationTypes_0005;
     vt : Word;
 end;
 PtagSAFEARRAYBOUND = ^tagSAFEARRAYBOUND;

 tagARRAYDESC = packed record
     tdescElem : tagTYPEDESC;
     cDims : Word;
     rgbounds : PtagSAFEARRAYBOUND;
 end;
 tagSAFEARRAYBOUND = packed record
     cElements : LongWord;
     lLbound : Integer;
 end;
 PtagIDLDESC = ^tagIDLDESC;

 tagIDLDESC = packed record
     dwReserved : ULONG_PTR;
     wIDLFlags : Word;
 end;
 tagTYPEATTR = packed record
     GUID : TGUID;
     lcid : LongWord;
     dwReserved : LongWord;
     memidConstructor : Integer;
     memidDestructor : Integer;
     lpstrSchema : PWideChar;
     cbSizeInstance : LongWord;
     typekind : tagTYPEKIND;
     cFuncs : Word;
     cVars : Word;
     cImplTypes : Word;
     cbSizeVft : Word;
     cbAlignment : Word;
     wTypeFlags : Word;
     wMajorVerNum : Word;
     wMinorVerNum : Word;
     tdescAlias : tagTYPEDESC;
     idldescType : tagIDLDESC;
 end;
 ULONG_PTR = LongWord;
 DWORD = LongWord;
 PtagFUNCDESC = ^tagFUNCDESC;

 PtagELEMDESC = ^tagELEMDESC;

 PtagPARAMDESC = ^tagPARAMDESC;

 PtagPARAMDESCEX = ^tagPARAMDESCEX;

 tagPARAMDESC = packed record
     pparamdescex : PtagPARAMDESCEX;
     wParamFlags : Word;
 end;
 tagELEMDESC = packed record
     tdesc : tagTYPEDESC;
     paramdesc : tagPARAMDESC;
 end;
 tagFUNCDESC = packed record
     memid : Integer;
     lprgscode : PSCODE;
     lprgelemdescParam : PtagELEMDESC;
     funckind : tagFUNCKIND;
     invkind : tagINVOKEKIND;
     callconv : tagCALLCONV;
     cParams : Smallint;
     cParamsOpt : Smallint;
     oVft : Smallint;
     cScodes : Smallint;
     elemdescFunc : tagELEMDESC;
     wFuncFlags : Word;
 end;
 tagPARAMDESCEX = packed record
     cBytes : LongWord;
     varDefaultValue : OleVariant;
 end;
 PtagVARDESC = ^tagVARDESC;

 P__MIDL_IOleAutomationTypes_0006 = ^__MIDL_IOleAutomationTypes_0006;

 __MIDL_IOleAutomationTypes_0006 =  record
    case Integer of
     0: (oInst : LongWord);
     1: (lpvarValue : POleVariant);
 end;
 tagVARDESC = packed record
     memid : Integer;
     lpstrSchema : PWideChar;
     DUMMYUNIONNAME : __MIDL_IOleAutomationTypes_0006;
     elemdescVar : tagELEMDESC;
     wVarFlags : Word;
     varkind : tagVARKIND;
 end;
 PtagTLIBATTR = ^tagTLIBATTR;

 tagTLIBATTR = packed record
     GUID : TGUID;
     lcid : LongWord;
     syskind : tagSYSKIND;
     wMajorVerNum : Word;
     wMinorVerNum : Word;
     wLibFlags : Word;
 end;
 ADS_SYSTEMFLAG_ENUM = __MIDL___MIDL_itf_ads_0001_0015_0001;
 ADS_GROUP_TYPE_ENUM = __MIDL___MIDL_itf_ads_0001_0021_0001;
 ADS_RIGHTS_ENUM = __MIDL___MIDL_itf_ads_0001_0043_0001;
 ADS_ACETYPE_ENUM = __MIDL___MIDL_itf_ads_0001_0043_0002;
 ADS_ACEFLAG_ENUM = __MIDL___MIDL_itf_ads_0001_0043_0003;
 ADS_FLAGTYPE_ENUM = __MIDL___MIDL_itf_ads_0001_0043_0004;
 ADS_SD_CONTROL_ENUM = __MIDL___MIDL_itf_ads_0001_0043_0005;
 ADS_SD_REVISION_ENUM = __MIDL___MIDL_itf_ads_0001_0043_0006;
 ADS_NAME_TYPE_ENUM = __MIDL___MIDL_itf_ads_0001_0044_0001;
 ADS_NAME_INITTYPE_ENUM = __MIDL___MIDL_itf_ads_0001_0044_0002;
 ADS_OPTION_ENUM = __MIDL___MIDL_itf_ads_0001_0058_0001;
 ADS_SECURITY_INFO_ENUM = __MIDL___MIDL_itf_ads_0001_0058_0002;
 ADS_SETTYPE_ENUM = __MIDL___MIDL_itf_ads_0001_0059_0001;
 ADS_FORMAT_ENUM = __MIDL___MIDL_itf_ads_0001_0059_0002;
 ADS_DISPLAY_ENUM = __MIDL___MIDL_itf_ads_0001_0059_0003;
 ADS_ESCAPE_MODE_ENUM = __MIDL___MIDL_itf_ads_0001_0059_0004;
 ADS_PATHTYPE_ENUM = __MIDL___MIDL_itf_ads_0001_0064_0001;
 ADS_SD_FORMAT_ENUM = __MIDL___MIDL_itf_ads_0001_0064_0002;

//interface declarations

// IADs : 

 IADs = interface(IDispatch)
   ['{FD8256D0-FD15-11CE-ABC4-02608C9E7553}']
   function Get_Name : WideString; safecall;
   function Get_Class_ : WideString; safecall;
   function Get_GUID : WideString; safecall;
   function Get_ADsPath : WideString; safecall;
   function Get_Parent : WideString; safecall;
   function Get_Schema : WideString; safecall;
    // GetInfo :  
   procedure GetInfo;safecall;
    // SetInfo :  
   procedure SetInfo;safecall;
    // Get :  
   function Get(bstrName:WideString):OleVariant;safecall;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);safecall;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;safecall;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);safecall;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);safecall;
    // Name :  
   property Name:WideString read Get_Name;
    // Class :  
   property Class_:WideString read Get_Class_;
    // GUID :  
   property GUID:WideString read Get_GUID;
    // ADsPath :  
   property ADsPath:WideString read Get_ADsPath;
    // Parent :  
   property Parent:WideString read Get_Parent;
    // Schema :  
   property Schema:WideString read Get_Schema;
  end;


// IADs : 

 IADsDisp = dispinterface
   ['{FD8256D0-FD15-11CE-ABC4-02608C9E7553}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetInfo :  
   procedure GetInfo;dispid 8;
    // SetInfo :  
   procedure SetInfo;dispid 9;
    // Get :  
   function Get(bstrName:WideString):OleVariant;dispid 10;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);dispid 11;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;dispid 12;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);dispid 13;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);dispid 14;
    // Name :  
   property Name:WideString  readonly dispid 2;
    // Class :  
   property Class_:WideString  readonly dispid 3;
    // GUID :  
   property GUID:WideString  readonly dispid 4;
    // ADsPath :  
   property ADsPath:WideString  readonly dispid 5;
    // Parent :  
   property Parent:WideString  readonly dispid 6;
    // Schema :  
   property Schema:WideString  readonly dispid 7;
  end;


// IADsContainer : 

 IADsContainer = interface(IDispatch)
   ['{001677D0-FD16-11CE-ABC4-02608C9E7553}']
   function Get_Count : Integer; safecall;
   function Get__NewEnum : IUnknown; safecall;
   function Get_Filter : OleVariant; safecall;
   procedure Set_Filter(const pVar:OleVariant); safecall;
   function Get_Hints : OleVariant; safecall;
   procedure Set_Hints(const pvFilter:OleVariant); safecall;
    // GetObject :  
   function GetObject(ClassName:WideString;RelativeName:WideString):IDispatch;safecall;
    // Create :  
   function Create(ClassName:WideString;RelativeName:WideString):IDispatch;safecall;
    // Delete :  
   procedure Delete(bstrClassName:WideString;bstrRelativeName:WideString);safecall;
    // CopyHere :  
   function CopyHere(SourceName:WideString;NewName:WideString):IDispatch;safecall;
    // MoveHere :  
   function MoveHere(SourceName:WideString;NewName:WideString):IDispatch;safecall;
    // Count :  
   property Count:Integer read Get_Count;
    // _NewEnum :  
   property _NewEnum:IUnknown read Get__NewEnum;
    // Filter :  
   property Filter:OleVariant read Get_Filter write Set_Filter;
    // Hints :  
   property Hints:OleVariant read Get_Hints write Set_Hints;
  end;


// IADsContainer : 

 IADsContainerDisp = dispinterface
   ['{001677D0-FD16-11CE-ABC4-02608C9E7553}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetObject :  
   function GetObject(ClassName:WideString;RelativeName:WideString):IDispatch;dispid 5;
    // Create :  
   function Create(ClassName:WideString;RelativeName:WideString):IDispatch;dispid 6;
    // Delete :  
   procedure Delete(bstrClassName:WideString;bstrRelativeName:WideString);dispid 7;
    // CopyHere :  
   function CopyHere(SourceName:WideString;NewName:WideString):IDispatch;dispid 8;
    // MoveHere :  
   function MoveHere(SourceName:WideString;NewName:WideString):IDispatch;dispid 9;
    // Count :  
   property Count:Integer  readonly dispid 2;
    // _NewEnum :  
   property _NewEnum:IUnknown  readonly dispid -4;
    // Filter :  
   property Filter:OleVariant dispid 3;
    // Hints :  
   property Hints:OleVariant dispid 4;
  end;


// IADsCollection : 

 IADsCollection = interface(IDispatch)
   ['{72B945E0-253B-11CF-A988-00AA006BC149}']
   function Get__NewEnum : IUnknown; safecall;
    // Add :  
   procedure Add(bstrName:WideString;vItem:OleVariant);safecall;
    // Remove :  
   procedure Remove(bstrItemToBeRemoved:WideString);safecall;
    // GetObject :  
   function GetObject(bstrName:WideString):OleVariant;safecall;
    // _NewEnum :  
   property _NewEnum:IUnknown read Get__NewEnum;
  end;


// IADsCollection : 

 IADsCollectionDisp = dispinterface
   ['{72B945E0-253B-11CF-A988-00AA006BC149}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Add :  
   procedure Add(bstrName:WideString;vItem:OleVariant);dispid 4;
    // Remove :  
   procedure Remove(bstrItemToBeRemoved:WideString);dispid 5;
    // GetObject :  
   function GetObject(bstrName:WideString):OleVariant;dispid 6;
    // _NewEnum :  
   property _NewEnum:IUnknown  readonly dispid -4;
  end;


// IADsMembers : 

 IADsMembers = interface(IDispatch)
   ['{451A0030-72EC-11CF-B03B-00AA006E0975}']
   function Get_Count : Integer; safecall;
   function Get__NewEnum : IUnknown; safecall;
   function Get_Filter : OleVariant; safecall;
   procedure Set_Filter(const pvFilter:OleVariant); safecall;
    // Count :  
   property Count:Integer read Get_Count;
    // _NewEnum :  
   property _NewEnum:IUnknown read Get__NewEnum;
    // Filter :  
   property Filter:OleVariant read Get_Filter write Set_Filter;
  end;


// IADsMembers : 

 IADsMembersDisp = dispinterface
   ['{451A0030-72EC-11CF-B03B-00AA006E0975}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Count :  
   property Count:Integer  readonly dispid 2;
    // _NewEnum :  
   property _NewEnum:IUnknown  readonly dispid -4;
    // Filter :  
   property Filter:OleVariant dispid 3;
  end;


// IADsPropertyList : 

 IADsPropertyList = interface(IDispatch)
   ['{C6F602B6-8F69-11D0-8528-00C04FD8D503}']
   function Get_PropertyCount : Integer; safecall;
    // Next :  
   function Next:OleVariant;safecall;
    // Skip :  
   procedure Skip(cElements:Integer);safecall;
    // Reset_ :  
   procedure Reset_;safecall;
    // Item :  
   function Item(varIndex:OleVariant):OleVariant;safecall;
    // GetPropertyItem :  
   function GetPropertyItem(bstrName:WideString;lnADsType:Integer):OleVariant;safecall;
    // PutPropertyItem :  
   procedure PutPropertyItem(varData:OleVariant);safecall;
    // ResetPropertyItem :  
   procedure ResetPropertyItem(varEntry:OleVariant);safecall;
    // PurgePropertyList :  
   procedure PurgePropertyList;safecall;
    // PropertyCount :  
   property PropertyCount:Integer read Get_PropertyCount;
  end;


// IADsPropertyList : 

 IADsPropertyListDisp = dispinterface
   ['{C6F602B6-8F69-11D0-8528-00C04FD8D503}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Next :  
   function Next:OleVariant;dispid 3;
    // Skip :  
   procedure Skip(cElements:Integer);dispid 4;
    // Reset_ :  
   procedure Reset_;dispid 5;
    // Item :  
   function Item(varIndex:OleVariant):OleVariant;dispid 0;
    // GetPropertyItem :  
   function GetPropertyItem(bstrName:WideString;lnADsType:Integer):OleVariant;dispid 6;
    // PutPropertyItem :  
   procedure PutPropertyItem(varData:OleVariant);dispid 7;
    // ResetPropertyItem :  
   procedure ResetPropertyItem(varEntry:OleVariant);dispid 8;
    // PurgePropertyList :  
   procedure PurgePropertyList;dispid 9;
    // PropertyCount :  
   property PropertyCount:Integer  readonly dispid 2;
  end;


// IADsPropertyEntry : 

 IADsPropertyEntry = interface(IDispatch)
   ['{05792C8E-941F-11D0-8529-00C04FD8D503}']
    // Clear :  
   procedure Clear;safecall;
   function Get_Name : WideString; safecall;
   procedure Set_Name(const retval:WideString); safecall;
   function Get_ADsType : Integer; safecall;
   procedure Set_ADsType(const retval:Integer); safecall;
   function Get_ControlCode : Integer; safecall;
   procedure Set_ControlCode(const retval:Integer); safecall;
   function Get_Values : OleVariant; safecall;
   procedure Set_Values(const retval:OleVariant); safecall;
    // Name :  
   property Name:WideString read Get_Name write Set_Name;
    // ADsType :  
   property ADsType:Integer read Get_ADsType write Set_ADsType;
    // ControlCode :  
   property ControlCode:Integer read Get_ControlCode write Set_ControlCode;
    // Values :  
   property Values:OleVariant read Get_Values write Set_Values;
  end;


// IADsPropertyEntry : 

 IADsPropertyEntryDisp = dispinterface
   ['{05792C8E-941F-11D0-8529-00C04FD8D503}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Clear :  
   procedure Clear;dispid 1;
    // Name :  
   property Name:WideString dispid 2;
    // ADsType :  
   property ADsType:Integer dispid 3;
    // ControlCode :  
   property ControlCode:Integer dispid 4;
    // Values :  
   property Values:OleVariant dispid 5;
  end;


// IADsPropertyValue : 

 IADsPropertyValue = interface(IDispatch)
   ['{79FA9AD0-A97C-11D0-8534-00C04FD8D503}']
    // Clear :  
   procedure Clear;safecall;
   function Get_ADsType : Integer; safecall;
   procedure Set_ADsType(const retval:Integer); safecall;
   function Get_DNString : WideString; safecall;
   procedure Set_DNString(const retval:WideString); safecall;
   function Get_CaseExactString : WideString; safecall;
   procedure Set_CaseExactString(const retval:WideString); safecall;
   function Get_CaseIgnoreString : WideString; safecall;
   procedure Set_CaseIgnoreString(const retval:WideString); safecall;
   function Get_PrintableString : WideString; safecall;
   procedure Set_PrintableString(const retval:WideString); safecall;
   function Get_NumericString : WideString; safecall;
   procedure Set_NumericString(const retval:WideString); safecall;
   function Get_Boolean_ : Integer; safecall;
   procedure Set_Boolean_(const retval:Integer); safecall;
   function Get_Integer_ : Integer; safecall;
   procedure Set_Integer_(const retval:Integer); safecall;
   function Get_OctetString : OleVariant; safecall;
   procedure Set_OctetString(const retval:OleVariant); safecall;
   function Get_SecurityDescriptor : IDispatch; safecall;
   procedure Set_SecurityDescriptor(const retval:IDispatch); safecall;
   function Get_LargeInteger : IDispatch; safecall;
   procedure Set_LargeInteger(const retval:IDispatch); safecall;
   function Get_UTCTime : TDateTime; safecall;
   procedure Set_UTCTime(const retval:TDateTime); safecall;
    // ADsType :  
   property ADsType:Integer read Get_ADsType write Set_ADsType;
    // DNString :  
   property DNString:WideString read Get_DNString write Set_DNString;
    // CaseExactString :  
   property CaseExactString:WideString read Get_CaseExactString write Set_CaseExactString;
    // CaseIgnoreString :  
   property CaseIgnoreString:WideString read Get_CaseIgnoreString write Set_CaseIgnoreString;
    // PrintableString :  
   property PrintableString:WideString read Get_PrintableString write Set_PrintableString;
    // NumericString :  
   property NumericString:WideString read Get_NumericString write Set_NumericString;
    // Boolean :  
   property Boolean_:Integer read Get_Boolean_ write Set_Boolean_;
    // Integer :  
   property Integer_:Integer read Get_Integer_ write Set_Integer_;
    // OctetString :  
   property OctetString:OleVariant read Get_OctetString write Set_OctetString;
    // SecurityDescriptor :  
   property SecurityDescriptor:IDispatch read Get_SecurityDescriptor write Set_SecurityDescriptor;
    // LargeInteger :  
   property LargeInteger:IDispatch read Get_LargeInteger write Set_LargeInteger;
    // UTCTime :  
   property UTCTime:TDateTime read Get_UTCTime write Set_UTCTime;
  end;


// IADsPropertyValue : 

 IADsPropertyValueDisp = dispinterface
   ['{79FA9AD0-A97C-11D0-8534-00C04FD8D503}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Clear :  
   procedure Clear;dispid 1;
    // ADsType :  
   property ADsType:Integer dispid 2;
    // DNString :  
   property DNString:WideString dispid 3;
    // CaseExactString :  
   property CaseExactString:WideString dispid 4;
    // CaseIgnoreString :  
   property CaseIgnoreString:WideString dispid 5;
    // PrintableString :  
   property PrintableString:WideString dispid 6;
    // NumericString :  
   property NumericString:WideString dispid 7;
    // Boolean :  
   property Boolean_:Integer dispid 8;
    // Integer :  
   property Integer_:Integer dispid 9;
    // OctetString :  
   property OctetString:OleVariant dispid 10;
    // SecurityDescriptor :  
   property SecurityDescriptor:IDispatch dispid 11;
    // LargeInteger :  
   property LargeInteger:IDispatch dispid 12;
    // UTCTime :  
   property UTCTime:TDateTime dispid 13;
  end;


// IADsPropertyValue2 : 

 IADsPropertyValue2 = interface(IDispatch)
   ['{306E831C-5BC7-11D1-A3B8-00C04FB950DC}']
    // GetObjectProperty :  
   function GetObjectProperty(var lnADsType:Integer):OleVariant;safecall;
    // PutObjectProperty :  
   procedure PutObjectProperty(lnADsType:Integer;vProp:OleVariant);safecall;
  end;


// IADsPropertyValue2 : 

 IADsPropertyValue2Disp = dispinterface
   ['{306E831C-5BC7-11D1-A3B8-00C04FB950DC}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetObjectProperty :  
   function GetObjectProperty(var lnADsType:Integer):OleVariant;dispid 1;
    // PutObjectProperty :  
   procedure PutObjectProperty(lnADsType:Integer;vProp:OleVariant);dispid 2;
  end;


// IPrivateDispatch : 

 IPrivateDispatch = interface(IUnknown)
   ['{86AB4BBE-65F6-11D1-8C13-00C04FD8D503}']
    // ADSIInitializeDispatchManager :  
   function ADSIInitializeDispatchManager(dwExtensionId:Integer):HRESULT;stdcall;
    // ADSIGetTypeInfoCount :  
   function ADSIGetTypeInfoCount(out pctinfo:UInt):HRESULT;stdcall;
    // ADSIGetTypeInfo :  
   function ADSIGetTypeInfo(itinfo:UInt;lcid:LongWord;out ppTInfo:ITypeInfo):HRESULT;stdcall;
    // ADSIGetIDsOfNames :  
   function ADSIGetIDsOfNames(var riid:GUID;var rgszNames:PWord;cNames:UInt;lcid:LongWord;out rgdispid:Integer):HRESULT;stdcall;
    // ADSIInvoke :  
   function ADSIInvoke(dispidMember:Integer;var riid:GUID;lcid:LongWord;wFlags:Word;var pdispparams:DISPPARAMS;out pvarResult:OleVariant;out pexcepinfo:EXCEPINFO;out puArgErr:UInt):HRESULT;stdcall;
  end;


// ITypeInfo : 

 ITypeInfo = interface(IUnknown)
   ['{00020401-0000-0000-C000-000000000046}']
    // RemoteGetTypeAttr :  
   function RemoteGetTypeAttr(out ppTypeAttr:PtagTYPEATTR;out pDummy:DWORD):HRESULT;stdcall;
    // GetTypeComp :  
   function GetTypeComp(out ppTComp:ITypeComp):HRESULT;stdcall;
    // RemoteGetFuncDesc :  
   function RemoteGetFuncDesc(index:UInt;out ppFuncDesc:PtagFUNCDESC;out pDummy:DWORD):HRESULT;stdcall;
    // RemoteGetVarDesc :  
   function RemoteGetVarDesc(index:UInt;out ppVarDesc:PtagVARDESC;out pDummy:DWORD):HRESULT;stdcall;
    // RemoteGetNames :  
   function RemoteGetNames(memid:Integer;out rgBstrNames:WideString;cMaxNames:UInt;out pcNames:UInt):HRESULT;stdcall;
    // GetRefTypeOfImplType :  
   function GetRefTypeOfImplType(index:UInt;out pRefType:LongWord):HRESULT;stdcall;
    // GetImplTypeFlags :  
   function GetImplTypeFlags(index:UInt;out pImplTypeFlags:SYSINT):HRESULT;stdcall;
    // LocalGetIDsOfNames :  
   function LocalGetIDsOfNames:HRESULT;stdcall;
    // LocalInvoke :  
   function LocalInvoke:HRESULT;stdcall;
    // RemoteGetDocumentation :  
   function RemoteGetDocumentation(memid:Integer;refPtrFlags:LongWord;out pBstrName:WideString;out pBstrDocString:WideString;out pdwHelpContext:LongWord;out pBstrHelpFile:WideString):HRESULT;stdcall;
    // RemoteGetDllEntry :  
   function RemoteGetDllEntry(memid:Integer;invkind:tagINVOKEKIND;refPtrFlags:LongWord;out pBstrDllName:WideString;out pBstrName:WideString;out pwOrdinal:Word):HRESULT;stdcall;
    // GetRefTypeInfo :  
   function GetRefTypeInfo(hreftype:LongWord;out ppTInfo:ITypeInfo):HRESULT;stdcall;
    // LocalAddressOfMember :  
   function LocalAddressOfMember:HRESULT;stdcall;
    // RemoteCreateInstance :  
   function RemoteCreateInstance(var riid:GUID;out ppvObj:IUnknown):HRESULT;stdcall;
    // GetMops :  
   function GetMops(memid:Integer;out pBstrMops:WideString):HRESULT;stdcall;
    // RemoteGetContainingTypeLib :  
   function RemoteGetContainingTypeLib(out ppTLib:ITypeLib;out pIndex:UInt):HRESULT;stdcall;
    // LocalReleaseTypeAttr :  
   function LocalReleaseTypeAttr:HRESULT;stdcall;
    // LocalReleaseFuncDesc :  
   function LocalReleaseFuncDesc:HRESULT;stdcall;
    // LocalReleaseVarDesc :  
   function LocalReleaseVarDesc:HRESULT;stdcall;
  end;


// ITypeComp : 

 ITypeComp = interface(IUnknown)
   ['{00020403-0000-0000-C000-000000000046}']
    // RemoteBind :  
   function RemoteBind(szName:PWideChar;lHashVal:LongWord;wFlags:Word;out ppTInfo:ITypeInfo;out pDescKind:tagDESCKIND;out ppFuncDesc:PtagFUNCDESC;out ppVarDesc:PtagVARDESC;out ppTypeComp:ITypeComp;out pDummy:DWORD):HRESULT;stdcall;
    // RemoteBindType :  
   function RemoteBindType(szName:PWideChar;lHashVal:LongWord;out ppTInfo:ITypeInfo):HRESULT;stdcall;
  end;


// ITypeLib : 

 ITypeLib = interface(IUnknown)
   ['{00020402-0000-0000-C000-000000000046}']
    // RemoteGetTypeInfoCount :  
   function RemoteGetTypeInfoCount(out pctinfo:UInt):HRESULT;stdcall;
    // GetTypeInfo :  
   function GetTypeInfo(index:UInt;out ppTInfo:ITypeInfo):HRESULT;stdcall;
    // GetTypeInfoType :  
   function GetTypeInfoType(index:UInt;out pTKind:tagTYPEKIND):HRESULT;stdcall;
    // GetTypeInfoOfGuid :  
   function GetTypeInfoOfGuid(var GUID:GUID;out ppTInfo:ITypeInfo):HRESULT;stdcall;
    // RemoteGetLibAttr :  
   function RemoteGetLibAttr(out ppTLibAttr:PtagTLIBATTR;out pDummy:DWORD):HRESULT;stdcall;
    // GetTypeComp :  
   function GetTypeComp(out ppTComp:ITypeComp):HRESULT;stdcall;
    // RemoteGetDocumentation :  
   function RemoteGetDocumentation(index:SYSINT;refPtrFlags:LongWord;out pBstrName:WideString;out pBstrDocString:WideString;out pdwHelpContext:LongWord;out pBstrHelpFile:WideString):HRESULT;stdcall;
    // RemoteIsName :  
   function RemoteIsName(szNameBuf:PWideChar;lHashVal:LongWord;out pfName:Integer;out pBstrLibName:WideString):HRESULT;stdcall;
    // RemoteFindName :  
   function RemoteFindName(szNameBuf:PWideChar;lHashVal:LongWord;out ppTInfo:ITypeInfo;out rgMemId:Integer;var pcFound:Word;out pBstrLibName:WideString):HRESULT;stdcall;
    // LocalReleaseTLibAttr :  
   function LocalReleaseTLibAttr:HRESULT;stdcall;
  end;


// IPrivateUnknown : 

 IPrivateUnknown = interface(IUnknown)
   ['{89126BAB-6EAD-11D1-8C18-00C04FD8D503}']
    // ADSIInitializeObject :  
   function ADSIInitializeObject(lpszUserName:WideString;lpszPassword:WideString;lnReserved:Integer):HRESULT;stdcall;
    // ADSIReleaseObject :  
   function ADSIReleaseObject:HRESULT;stdcall;
  end;


// IADsExtension : 

 IADsExtension = interface(IUnknown)
   ['{3D35553C-D2B0-11D1-B17B-0000F87593A0}']
    // Operate :  
   function Operate(dwCode:LongWord;varData1:OleVariant;varData2:OleVariant;varData3:OleVariant):HRESULT;stdcall;
    // PrivateGetIDsOfNames :  
   function PrivateGetIDsOfNames(var riid:GUID;var rgszNames:PWord;cNames:UInt;lcid:LongWord;out rgdispid:Integer):HRESULT;stdcall;
    // PrivateInvoke :  
   function PrivateInvoke(dispidMember:Integer;var riid:GUID;lcid:LongWord;wFlags:Word;var pdispparams:DISPPARAMS;out pvarResult:OleVariant;out pexcepinfo:EXCEPINFO;out puArgErr:UInt):HRESULT;stdcall;
  end;


// IADsDeleteOps : 

 IADsDeleteOps = interface(IDispatch)
   ['{B2BD0902-8878-11D1-8C21-00C04FD8D503}']
    // DeleteObject :  
   procedure DeleteObject(lnFlags:Integer);safecall;
  end;


// IADsDeleteOps : 

 IADsDeleteOpsDisp = dispinterface
   ['{B2BD0902-8878-11D1-8C21-00C04FD8D503}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // DeleteObject :  
   procedure DeleteObject(lnFlags:Integer);dispid 2;
  end;


// IADsNamespaces : 

 IADsNamespaces = interface(IADs)
   ['{28B96BA0-B330-11CF-A9AD-00AA006BC149}']
   function Get_DefaultContainer : WideString; safecall;
   procedure Set_DefaultContainer(const retval:WideString); safecall;
    // DefaultContainer :  
   property DefaultContainer:WideString read Get_DefaultContainer write Set_DefaultContainer;
  end;


// IADsNamespaces : 

 IADsNamespacesDisp = dispinterface
   ['{28B96BA0-B330-11CF-A9AD-00AA006BC149}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetInfo :  
   procedure GetInfo;dispid 8;
    // SetInfo :  
   procedure SetInfo;dispid 9;
    // Get :  
   function Get(bstrName:WideString):OleVariant;dispid 10;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);dispid 11;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;dispid 12;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);dispid 13;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);dispid 14;
    // Name :  
   property Name:WideString  readonly dispid 2;
    // Class :  
   property Class_:WideString  readonly dispid 3;
    // GUID :  
   property GUID:WideString  readonly dispid 4;
    // ADsPath :  
   property ADsPath:WideString  readonly dispid 5;
    // Parent :  
   property Parent:WideString  readonly dispid 6;
    // Schema :  
   property Schema:WideString  readonly dispid 7;
    // DefaultContainer :  
   property DefaultContainer:WideString dispid 1;
  end;


// IADsClass : 

 IADsClass = interface(IADs)
   ['{C8F93DD0-4AE0-11CF-9E73-00AA004A5691}']
   function Get_PrimaryInterface : WideString; safecall;
   function Get_CLSID : WideString; safecall;
   procedure Set_CLSID(const retval:WideString); safecall;
   function Get_OID : WideString; safecall;
   procedure Set_OID(const retval:WideString); safecall;
   function Get_Abstract : WordBool; safecall;
   procedure Set_Abstract(const retval:WordBool); safecall;
   function Get_Auxiliary : WordBool; safecall;
   procedure Set_Auxiliary(const retval:WordBool); safecall;
   function Get_MandatoryProperties : OleVariant; safecall;
   procedure Set_MandatoryProperties(const retval:OleVariant); safecall;
   function Get_OptionalProperties : OleVariant; safecall;
   procedure Set_OptionalProperties(const retval:OleVariant); safecall;
   function Get_NamingProperties : OleVariant; safecall;
   procedure Set_NamingProperties(const retval:OleVariant); safecall;
   function Get_DerivedFrom : OleVariant; safecall;
   procedure Set_DerivedFrom(const retval:OleVariant); safecall;
   function Get_AuxDerivedFrom : OleVariant; safecall;
   procedure Set_AuxDerivedFrom(const retval:OleVariant); safecall;
   function Get_PossibleSuperiors : OleVariant; safecall;
   procedure Set_PossibleSuperiors(const retval:OleVariant); safecall;
   function Get_Containment : OleVariant; safecall;
   procedure Set_Containment(const retval:OleVariant); safecall;
   function Get_Container : WordBool; safecall;
   procedure Set_Container(const retval:WordBool); safecall;
   function Get_HelpFileName : WideString; safecall;
   procedure Set_HelpFileName(const retval:WideString); safecall;
   function Get_HelpFileContext : Integer; safecall;
   procedure Set_HelpFileContext(const retval:Integer); safecall;
    // Qualifiers :  
   function Qualifiers:IADsCollection;safecall;
    // PrimaryInterface :  
   property PrimaryInterface:WideString read Get_PrimaryInterface;
    // CLSID :  
   property CLSID:WideString read Get_CLSID write Set_CLSID;
    // OID :  
   property OID:WideString read Get_OID write Set_OID;
    // Abstract :  
   property Abstract:WordBool read Get_Abstract write Set_Abstract;
    // Auxiliary :  
   property Auxiliary:WordBool read Get_Auxiliary write Set_Auxiliary;
    // MandatoryProperties :  
   property MandatoryProperties:OleVariant read Get_MandatoryProperties write Set_MandatoryProperties;
    // OptionalProperties :  
   property OptionalProperties:OleVariant read Get_OptionalProperties write Set_OptionalProperties;
    // NamingProperties :  
   property NamingProperties:OleVariant read Get_NamingProperties write Set_NamingProperties;
    // DerivedFrom :  
   property DerivedFrom:OleVariant read Get_DerivedFrom write Set_DerivedFrom;
    // AuxDerivedFrom :  
   property AuxDerivedFrom:OleVariant read Get_AuxDerivedFrom write Set_AuxDerivedFrom;
    // PossibleSuperiors :  
   property PossibleSuperiors:OleVariant read Get_PossibleSuperiors write Set_PossibleSuperiors;
    // Containment :  
   property Containment:OleVariant read Get_Containment write Set_Containment;
    // Container :  
   property Container:WordBool read Get_Container write Set_Container;
    // HelpFileName :  
   property HelpFileName:WideString read Get_HelpFileName write Set_HelpFileName;
    // HelpFileContext :  
   property HelpFileContext:Integer read Get_HelpFileContext write Set_HelpFileContext;
  end;


// IADsClass : 

 IADsClassDisp = dispinterface
   ['{C8F93DD0-4AE0-11CF-9E73-00AA004A5691}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetInfo :  
   procedure GetInfo;dispid 8;
    // SetInfo :  
   procedure SetInfo;dispid 9;
    // Get :  
   function Get(bstrName:WideString):OleVariant;dispid 10;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);dispid 11;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;dispid 12;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);dispid 13;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);dispid 14;
    // Qualifiers :  
   function Qualifiers:IADsCollection;dispid 25;
    // Name :  
   property Name:WideString  readonly dispid 2;
    // Class :  
   property Class_:WideString  readonly dispid 3;
    // GUID :  
   property GUID:WideString  readonly dispid 4;
    // ADsPath :  
   property ADsPath:WideString  readonly dispid 5;
    // Parent :  
   property Parent:WideString  readonly dispid 6;
    // Schema :  
   property Schema:WideString  readonly dispid 7;
    // PrimaryInterface :  
   property PrimaryInterface:WideString  readonly dispid 15;
    // CLSID :  
   property CLSID:WideString dispid 16;
    // OID :  
   property OID:WideString dispid 17;
    // Abstract :  
   property Abstract:WordBool dispid 18;
    // Auxiliary :  
   property Auxiliary:WordBool dispid 26;
    // MandatoryProperties :  
   property MandatoryProperties:OleVariant dispid 19;
    // OptionalProperties :  
   property OptionalProperties:OleVariant dispid 29;
    // NamingProperties :  
   property NamingProperties:OleVariant dispid 30;
    // DerivedFrom :  
   property DerivedFrom:OleVariant dispid 20;
    // AuxDerivedFrom :  
   property AuxDerivedFrom:OleVariant dispid 27;
    // PossibleSuperiors :  
   property PossibleSuperiors:OleVariant dispid 28;
    // Containment :  
   property Containment:OleVariant dispid 21;
    // Container :  
   property Container:WordBool dispid 22;
    // HelpFileName :  
   property HelpFileName:WideString dispid 23;
    // HelpFileContext :  
   property HelpFileContext:Integer dispid 24;
  end;


// IADsProperty : 

 IADsProperty = interface(IADs)
   ['{C8F93DD3-4AE0-11CF-9E73-00AA004A5691}']
   function Get_OID : WideString; safecall;
   procedure Set_OID(const retval:WideString); safecall;
   function Get_Syntax : WideString; safecall;
   procedure Set_Syntax(const retval:WideString); safecall;
   function Get_MaxRange : Integer; safecall;
   procedure Set_MaxRange(const retval:Integer); safecall;
   function Get_MinRange : Integer; safecall;
   procedure Set_MinRange(const retval:Integer); safecall;
   function Get_MultiValued : WordBool; safecall;
   procedure Set_MultiValued(const retval:WordBool); safecall;
    // Qualifiers :  
   function Qualifiers:IADsCollection;safecall;
    // OID :  
   property OID:WideString read Get_OID write Set_OID;
    // Syntax :  
   property Syntax:WideString read Get_Syntax write Set_Syntax;
    // MaxRange :  
   property MaxRange:Integer read Get_MaxRange write Set_MaxRange;
    // MinRange :  
   property MinRange:Integer read Get_MinRange write Set_MinRange;
    // MultiValued :  
   property MultiValued:WordBool read Get_MultiValued write Set_MultiValued;
  end;


// IADsProperty : 

 IADsPropertyDisp = dispinterface
   ['{C8F93DD3-4AE0-11CF-9E73-00AA004A5691}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetInfo :  
   procedure GetInfo;dispid 8;
    // SetInfo :  
   procedure SetInfo;dispid 9;
    // Get :  
   function Get(bstrName:WideString):OleVariant;dispid 10;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);dispid 11;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;dispid 12;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);dispid 13;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);dispid 14;
    // Qualifiers :  
   function Qualifiers:IADsCollection;dispid 22;
    // Name :  
   property Name:WideString  readonly dispid 2;
    // Class :  
   property Class_:WideString  readonly dispid 3;
    // GUID :  
   property GUID:WideString  readonly dispid 4;
    // ADsPath :  
   property ADsPath:WideString  readonly dispid 5;
    // Parent :  
   property Parent:WideString  readonly dispid 6;
    // Schema :  
   property Schema:WideString  readonly dispid 7;
    // OID :  
   property OID:WideString dispid 17;
    // Syntax :  
   property Syntax:WideString dispid 18;
    // MaxRange :  
   property MaxRange:Integer dispid 19;
    // MinRange :  
   property MinRange:Integer dispid 20;
    // MultiValued :  
   property MultiValued:WordBool dispid 21;
  end;


// IADsSyntax : 

 IADsSyntax = interface(IADs)
   ['{C8F93DD2-4AE0-11CF-9E73-00AA004A5691}']
   function Get_OleAutoDataType : Integer; safecall;
   procedure Set_OleAutoDataType(const retval:Integer); safecall;
    // OleAutoDataType :  
   property OleAutoDataType:Integer read Get_OleAutoDataType write Set_OleAutoDataType;
  end;


// IADsSyntax : 

 IADsSyntaxDisp = dispinterface
   ['{C8F93DD2-4AE0-11CF-9E73-00AA004A5691}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetInfo :  
   procedure GetInfo;dispid 8;
    // SetInfo :  
   procedure SetInfo;dispid 9;
    // Get :  
   function Get(bstrName:WideString):OleVariant;dispid 10;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);dispid 11;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;dispid 12;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);dispid 13;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);dispid 14;
    // Name :  
   property Name:WideString  readonly dispid 2;
    // Class :  
   property Class_:WideString  readonly dispid 3;
    // GUID :  
   property GUID:WideString  readonly dispid 4;
    // ADsPath :  
   property ADsPath:WideString  readonly dispid 5;
    // Parent :  
   property Parent:WideString  readonly dispid 6;
    // Schema :  
   property Schema:WideString  readonly dispid 7;
    // OleAutoDataType :  
   property OleAutoDataType:Integer dispid 15;
  end;


// IADsLocality : 

 IADsLocality = interface(IADs)
   ['{A05E03A2-EFFE-11CF-8ABC-00C04FD8D503}']
   function Get_Description : WideString; safecall;
   procedure Set_Description(const retval:WideString); safecall;
   function Get_LocalityName : WideString; safecall;
   procedure Set_LocalityName(const retval:WideString); safecall;
   function Get_PostalAddress : WideString; safecall;
   procedure Set_PostalAddress(const retval:WideString); safecall;
   function Get_SeeAlso : OleVariant; safecall;
   procedure Set_SeeAlso(const retval:OleVariant); safecall;
    // Description :  
   property Description:WideString read Get_Description write Set_Description;
    // LocalityName :  
   property LocalityName:WideString read Get_LocalityName write Set_LocalityName;
    // PostalAddress :  
   property PostalAddress:WideString read Get_PostalAddress write Set_PostalAddress;
    // SeeAlso :  
   property SeeAlso:OleVariant read Get_SeeAlso write Set_SeeAlso;
  end;


// IADsLocality : 

 IADsLocalityDisp = dispinterface
   ['{A05E03A2-EFFE-11CF-8ABC-00C04FD8D503}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetInfo :  
   procedure GetInfo;dispid 8;
    // SetInfo :  
   procedure SetInfo;dispid 9;
    // Get :  
   function Get(bstrName:WideString):OleVariant;dispid 10;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);dispid 11;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;dispid 12;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);dispid 13;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);dispid 14;
    // Name :  
   property Name:WideString  readonly dispid 2;
    // Class :  
   property Class_:WideString  readonly dispid 3;
    // GUID :  
   property GUID:WideString  readonly dispid 4;
    // ADsPath :  
   property ADsPath:WideString  readonly dispid 5;
    // Parent :  
   property Parent:WideString  readonly dispid 6;
    // Schema :  
   property Schema:WideString  readonly dispid 7;
    // Description :  
   property Description:WideString dispid 15;
    // LocalityName :  
   property LocalityName:WideString dispid 16;
    // PostalAddress :  
   property PostalAddress:WideString dispid 17;
    // SeeAlso :  
   property SeeAlso:OleVariant dispid 18;
  end;


// IADsO : 

 IADsO = interface(IADs)
   ['{A1CD2DC6-EFFE-11CF-8ABC-00C04FD8D503}']
   function Get_Description : WideString; safecall;
   procedure Set_Description(const retval:WideString); safecall;
   function Get_LocalityName : WideString; safecall;
   procedure Set_LocalityName(const retval:WideString); safecall;
   function Get_PostalAddress : WideString; safecall;
   procedure Set_PostalAddress(const retval:WideString); safecall;
   function Get_TelephoneNumber : WideString; safecall;
   procedure Set_TelephoneNumber(const retval:WideString); safecall;
   function Get_FaxNumber : WideString; safecall;
   procedure Set_FaxNumber(const retval:WideString); safecall;
   function Get_SeeAlso : OleVariant; safecall;
   procedure Set_SeeAlso(const retval:OleVariant); safecall;
    // Description :  
   property Description:WideString read Get_Description write Set_Description;
    // LocalityName :  
   property LocalityName:WideString read Get_LocalityName write Set_LocalityName;
    // PostalAddress :  
   property PostalAddress:WideString read Get_PostalAddress write Set_PostalAddress;
    // TelephoneNumber :  
   property TelephoneNumber:WideString read Get_TelephoneNumber write Set_TelephoneNumber;
    // FaxNumber :  
   property FaxNumber:WideString read Get_FaxNumber write Set_FaxNumber;
    // SeeAlso :  
   property SeeAlso:OleVariant read Get_SeeAlso write Set_SeeAlso;
  end;


// IADsO : 

 IADsODisp = dispinterface
   ['{A1CD2DC6-EFFE-11CF-8ABC-00C04FD8D503}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetInfo :  
   procedure GetInfo;dispid 8;
    // SetInfo :  
   procedure SetInfo;dispid 9;
    // Get :  
   function Get(bstrName:WideString):OleVariant;dispid 10;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);dispid 11;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;dispid 12;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);dispid 13;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);dispid 14;
    // Name :  
   property Name:WideString  readonly dispid 2;
    // Class :  
   property Class_:WideString  readonly dispid 3;
    // GUID :  
   property GUID:WideString  readonly dispid 4;
    // ADsPath :  
   property ADsPath:WideString  readonly dispid 5;
    // Parent :  
   property Parent:WideString  readonly dispid 6;
    // Schema :  
   property Schema:WideString  readonly dispid 7;
    // Description :  
   property Description:WideString dispid 15;
    // LocalityName :  
   property LocalityName:WideString dispid 16;
    // PostalAddress :  
   property PostalAddress:WideString dispid 17;
    // TelephoneNumber :  
   property TelephoneNumber:WideString dispid 18;
    // FaxNumber :  
   property FaxNumber:WideString dispid 19;
    // SeeAlso :  
   property SeeAlso:OleVariant dispid 20;
  end;


// IADsOU : 

 IADsOU = interface(IADs)
   ['{A2F733B8-EFFE-11CF-8ABC-00C04FD8D503}']
   function Get_Description : WideString; safecall;
   procedure Set_Description(const retval:WideString); safecall;
   function Get_LocalityName : WideString; safecall;
   procedure Set_LocalityName(const retval:WideString); safecall;
   function Get_PostalAddress : WideString; safecall;
   procedure Set_PostalAddress(const retval:WideString); safecall;
   function Get_TelephoneNumber : WideString; safecall;
   procedure Set_TelephoneNumber(const retval:WideString); safecall;
   function Get_FaxNumber : WideString; safecall;
   procedure Set_FaxNumber(const retval:WideString); safecall;
   function Get_SeeAlso : OleVariant; safecall;
   procedure Set_SeeAlso(const retval:OleVariant); safecall;
   function Get_BusinessCategory : WideString; safecall;
   procedure Set_BusinessCategory(const retval:WideString); safecall;
    // Description :  
   property Description:WideString read Get_Description write Set_Description;
    // LocalityName :  
   property LocalityName:WideString read Get_LocalityName write Set_LocalityName;
    // PostalAddress :  
   property PostalAddress:WideString read Get_PostalAddress write Set_PostalAddress;
    // TelephoneNumber :  
   property TelephoneNumber:WideString read Get_TelephoneNumber write Set_TelephoneNumber;
    // FaxNumber :  
   property FaxNumber:WideString read Get_FaxNumber write Set_FaxNumber;
    // SeeAlso :  
   property SeeAlso:OleVariant read Get_SeeAlso write Set_SeeAlso;
    // BusinessCategory :  
   property BusinessCategory:WideString read Get_BusinessCategory write Set_BusinessCategory;
  end;


// IADsOU : 

 IADsOUDisp = dispinterface
   ['{A2F733B8-EFFE-11CF-8ABC-00C04FD8D503}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetInfo :  
   procedure GetInfo;dispid 8;
    // SetInfo :  
   procedure SetInfo;dispid 9;
    // Get :  
   function Get(bstrName:WideString):OleVariant;dispid 10;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);dispid 11;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;dispid 12;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);dispid 13;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);dispid 14;
    // Name :  
   property Name:WideString  readonly dispid 2;
    // Class :  
   property Class_:WideString  readonly dispid 3;
    // GUID :  
   property GUID:WideString  readonly dispid 4;
    // ADsPath :  
   property ADsPath:WideString  readonly dispid 5;
    // Parent :  
   property Parent:WideString  readonly dispid 6;
    // Schema :  
   property Schema:WideString  readonly dispid 7;
    // Description :  
   property Description:WideString dispid 15;
    // LocalityName :  
   property LocalityName:WideString dispid 16;
    // PostalAddress :  
   property PostalAddress:WideString dispid 17;
    // TelephoneNumber :  
   property TelephoneNumber:WideString dispid 18;
    // FaxNumber :  
   property FaxNumber:WideString dispid 19;
    // SeeAlso :  
   property SeeAlso:OleVariant dispid 20;
    // BusinessCategory :  
   property BusinessCategory:WideString dispid 21;
  end;


// IADsDomain : 

 IADsDomain = interface(IADs)
   ['{00E4C220-FD16-11CE-ABC4-02608C9E7553}']
   function Get_IsWorkgroup : WordBool; safecall;
   function Get_MinPasswordLength : Integer; safecall;
   procedure Set_MinPasswordLength(const retval:Integer); safecall;
   function Get_MinPasswordAge : Integer; safecall;
   procedure Set_MinPasswordAge(const retval:Integer); safecall;
   function Get_MaxPasswordAge : Integer; safecall;
   procedure Set_MaxPasswordAge(const retval:Integer); safecall;
   function Get_MaxBadPasswordsAllowed : Integer; safecall;
   procedure Set_MaxBadPasswordsAllowed(const retval:Integer); safecall;
   function Get_PasswordHistoryLength : Integer; safecall;
   procedure Set_PasswordHistoryLength(const retval:Integer); safecall;
   function Get_PasswordAttributes : Integer; safecall;
   procedure Set_PasswordAttributes(const retval:Integer); safecall;
   function Get_AutoUnlockInterval : Integer; safecall;
   procedure Set_AutoUnlockInterval(const retval:Integer); safecall;
   function Get_LockoutObservationInterval : Integer; safecall;
   procedure Set_LockoutObservationInterval(const retval:Integer); safecall;
    // IsWorkgroup :  
   property IsWorkgroup:WordBool read Get_IsWorkgroup;
    // MinPasswordLength :  
   property MinPasswordLength:Integer read Get_MinPasswordLength write Set_MinPasswordLength;
    // MinPasswordAge :  
   property MinPasswordAge:Integer read Get_MinPasswordAge write Set_MinPasswordAge;
    // MaxPasswordAge :  
   property MaxPasswordAge:Integer read Get_MaxPasswordAge write Set_MaxPasswordAge;
    // MaxBadPasswordsAllowed :  
   property MaxBadPasswordsAllowed:Integer read Get_MaxBadPasswordsAllowed write Set_MaxBadPasswordsAllowed;
    // PasswordHistoryLength :  
   property PasswordHistoryLength:Integer read Get_PasswordHistoryLength write Set_PasswordHistoryLength;
    // PasswordAttributes :  
   property PasswordAttributes:Integer read Get_PasswordAttributes write Set_PasswordAttributes;
    // AutoUnlockInterval :  
   property AutoUnlockInterval:Integer read Get_AutoUnlockInterval write Set_AutoUnlockInterval;
    // LockoutObservationInterval :  
   property LockoutObservationInterval:Integer read Get_LockoutObservationInterval write Set_LockoutObservationInterval;
  end;


// IADsDomain : 

 IADsDomainDisp = dispinterface
   ['{00E4C220-FD16-11CE-ABC4-02608C9E7553}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetInfo :  
   procedure GetInfo;dispid 8;
    // SetInfo :  
   procedure SetInfo;dispid 9;
    // Get :  
   function Get(bstrName:WideString):OleVariant;dispid 10;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);dispid 11;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;dispid 12;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);dispid 13;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);dispid 14;
    // Name :  
   property Name:WideString  readonly dispid 2;
    // Class :  
   property Class_:WideString  readonly dispid 3;
    // GUID :  
   property GUID:WideString  readonly dispid 4;
    // ADsPath :  
   property ADsPath:WideString  readonly dispid 5;
    // Parent :  
   property Parent:WideString  readonly dispid 6;
    // Schema :  
   property Schema:WideString  readonly dispid 7;
    // IsWorkgroup :  
   property IsWorkgroup:WordBool  readonly dispid 15;
    // MinPasswordLength :  
   property MinPasswordLength:Integer dispid 16;
    // MinPasswordAge :  
   property MinPasswordAge:Integer dispid 17;
    // MaxPasswordAge :  
   property MaxPasswordAge:Integer dispid 18;
    // MaxBadPasswordsAllowed :  
   property MaxBadPasswordsAllowed:Integer dispid 19;
    // PasswordHistoryLength :  
   property PasswordHistoryLength:Integer dispid 20;
    // PasswordAttributes :  
   property PasswordAttributes:Integer dispid 21;
    // AutoUnlockInterval :  
   property AutoUnlockInterval:Integer dispid 22;
    // LockoutObservationInterval :  
   property LockoutObservationInterval:Integer dispid 23;
  end;


// IADsComputer : 

 IADsComputer = interface(IADs)
   ['{EFE3CC70-1D9F-11CF-B1F3-02608C9E7553}']
   function Get_ComputerID : WideString; safecall;
   function Get_Site : WideString; safecall;
   function Get_Description : WideString; safecall;
   procedure Set_Description(const retval:WideString); safecall;
   function Get_Location : WideString; safecall;
   procedure Set_Location(const retval:WideString); safecall;
   function Get_PrimaryUser : WideString; safecall;
   procedure Set_PrimaryUser(const retval:WideString); safecall;
   function Get_Owner : WideString; safecall;
   procedure Set_Owner(const retval:WideString); safecall;
   function Get_Division : WideString; safecall;
   procedure Set_Division(const retval:WideString); safecall;
   function Get_Department : WideString; safecall;
   procedure Set_Department(const retval:WideString); safecall;
   function Get_Role : WideString; safecall;
   procedure Set_Role(const retval:WideString); safecall;
   function Get_OperatingSystem : WideString; safecall;
   procedure Set_OperatingSystem(const retval:WideString); safecall;
   function Get_OperatingSystemVersion : WideString; safecall;
   procedure Set_OperatingSystemVersion(const retval:WideString); safecall;
   function Get_Model : WideString; safecall;
   procedure Set_Model(const retval:WideString); safecall;
   function Get_Processor : WideString; safecall;
   procedure Set_Processor(const retval:WideString); safecall;
   function Get_ProcessorCount : WideString; safecall;
   procedure Set_ProcessorCount(const retval:WideString); safecall;
   function Get_MemorySize : WideString; safecall;
   procedure Set_MemorySize(const retval:WideString); safecall;
   function Get_StorageCapacity : WideString; safecall;
   procedure Set_StorageCapacity(const retval:WideString); safecall;
   function Get_NetAddresses : OleVariant; safecall;
   procedure Set_NetAddresses(const retval:OleVariant); safecall;
    // ComputerID :  
   property ComputerID:WideString read Get_ComputerID;
    // Site :  
   property Site:WideString read Get_Site;
    // Description :  
   property Description:WideString read Get_Description write Set_Description;
    // Location :  
   property Location:WideString read Get_Location write Set_Location;
    // PrimaryUser :  
   property PrimaryUser:WideString read Get_PrimaryUser write Set_PrimaryUser;
    // Owner :  
   property Owner:WideString read Get_Owner write Set_Owner;
    // Division :  
   property Division:WideString read Get_Division write Set_Division;
    // Department :  
   property Department:WideString read Get_Department write Set_Department;
    // Role :  
   property Role:WideString read Get_Role write Set_Role;
    // OperatingSystem :  
   property OperatingSystem:WideString read Get_OperatingSystem write Set_OperatingSystem;
    // OperatingSystemVersion :  
   property OperatingSystemVersion:WideString read Get_OperatingSystemVersion write Set_OperatingSystemVersion;
    // Model :  
   property Model:WideString read Get_Model write Set_Model;
    // Processor :  
   property Processor:WideString read Get_Processor write Set_Processor;
    // ProcessorCount :  
   property ProcessorCount:WideString read Get_ProcessorCount write Set_ProcessorCount;
    // MemorySize :  
   property MemorySize:WideString read Get_MemorySize write Set_MemorySize;
    // StorageCapacity :  
   property StorageCapacity:WideString read Get_StorageCapacity write Set_StorageCapacity;
    // NetAddresses :  
   property NetAddresses:OleVariant read Get_NetAddresses write Set_NetAddresses;
  end;


// IADsComputer : 

 IADsComputerDisp = dispinterface
   ['{EFE3CC70-1D9F-11CF-B1F3-02608C9E7553}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetInfo :  
   procedure GetInfo;dispid 8;
    // SetInfo :  
   procedure SetInfo;dispid 9;
    // Get :  
   function Get(bstrName:WideString):OleVariant;dispid 10;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);dispid 11;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;dispid 12;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);dispid 13;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);dispid 14;
    // Name :  
   property Name:WideString  readonly dispid 2;
    // Class :  
   property Class_:WideString  readonly dispid 3;
    // GUID :  
   property GUID:WideString  readonly dispid 4;
    // ADsPath :  
   property ADsPath:WideString  readonly dispid 5;
    // Parent :  
   property Parent:WideString  readonly dispid 6;
    // Schema :  
   property Schema:WideString  readonly dispid 7;
    // ComputerID :  
   property ComputerID:WideString  readonly dispid 16;
    // Site :  
   property Site:WideString  readonly dispid 18;
    // Description :  
   property Description:WideString dispid 19;
    // Location :  
   property Location:WideString dispid 20;
    // PrimaryUser :  
   property PrimaryUser:WideString dispid 21;
    // Owner :  
   property Owner:WideString dispid 22;
    // Division :  
   property Division:WideString dispid 23;
    // Department :  
   property Department:WideString dispid 24;
    // Role :  
   property Role:WideString dispid 25;
    // OperatingSystem :  
   property OperatingSystem:WideString dispid 26;
    // OperatingSystemVersion :  
   property OperatingSystemVersion:WideString dispid 27;
    // Model :  
   property Model:WideString dispid 28;
    // Processor :  
   property Processor:WideString dispid 29;
    // ProcessorCount :  
   property ProcessorCount:WideString dispid 30;
    // MemorySize :  
   property MemorySize:WideString dispid 31;
    // StorageCapacity :  
   property StorageCapacity:WideString dispid 32;
    // NetAddresses :  
   property NetAddresses:OleVariant dispid 17;
  end;


// IADsComputerOperations : 

 IADsComputerOperations = interface(IADs)
   ['{EF497680-1D9F-11CF-B1F3-02608C9E7553}']
    // Status :  
   function Status:IDispatch;safecall;
    // Shutdown :  
   procedure Shutdown(bReboot:WordBool);safecall;
  end;


// IADsComputerOperations : 

 IADsComputerOperationsDisp = dispinterface
   ['{EF497680-1D9F-11CF-B1F3-02608C9E7553}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetInfo :  
   procedure GetInfo;dispid 8;
    // SetInfo :  
   procedure SetInfo;dispid 9;
    // Get :  
   function Get(bstrName:WideString):OleVariant;dispid 10;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);dispid 11;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;dispid 12;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);dispid 13;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);dispid 14;
    // Status :  
   function Status:IDispatch;dispid 33;
    // Shutdown :  
   procedure Shutdown(bReboot:WordBool);dispid 34;
    // Name :  
   property Name:WideString  readonly dispid 2;
    // Class :  
   property Class_:WideString  readonly dispid 3;
    // GUID :  
   property GUID:WideString  readonly dispid 4;
    // ADsPath :  
   property ADsPath:WideString  readonly dispid 5;
    // Parent :  
   property Parent:WideString  readonly dispid 6;
    // Schema :  
   property Schema:WideString  readonly dispid 7;
  end;


// IADsGroup : 

 IADsGroup = interface(IADs)
   ['{27636B00-410F-11CF-B1FF-02608C9E7553}']
   function Get_Description : WideString; safecall;
   procedure Set_Description(const retval:WideString); safecall;
    // Members :  
   function Members:IADsMembers;safecall;
    // IsMember :  
   function IsMember(bstrMember:WideString):WordBool;safecall;
    // Add :  
   procedure Add(bstrNewItem:WideString);safecall;
    // Remove :  
   procedure Remove(bstrItemToBeRemoved:WideString);safecall;
    // Description :  
   property Description:WideString read Get_Description write Set_Description;
  end;


// IADsGroup : 

 IADsGroupDisp = dispinterface
   ['{27636B00-410F-11CF-B1FF-02608C9E7553}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetInfo :  
   procedure GetInfo;dispid 8;
    // SetInfo :  
   procedure SetInfo;dispid 9;
    // Get :  
   function Get(bstrName:WideString):OleVariant;dispid 10;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);dispid 11;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;dispid 12;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);dispid 13;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);dispid 14;
    // Members :  
   function Members:IADsMembers;dispid 16;
    // IsMember :  
   function IsMember(bstrMember:WideString):WordBool;dispid 17;
    // Add :  
   procedure Add(bstrNewItem:WideString);dispid 18;
    // Remove :  
   procedure Remove(bstrItemToBeRemoved:WideString);dispid 19;
    // Name :  
   property Name:WideString  readonly dispid 2;
    // Class :  
   property Class_:WideString  readonly dispid 3;
    // GUID :  
   property GUID:WideString  readonly dispid 4;
    // ADsPath :  
   property ADsPath:WideString  readonly dispid 5;
    // Parent :  
   property Parent:WideString  readonly dispid 6;
    // Schema :  
   property Schema:WideString  readonly dispid 7;
    // Description :  
   property Description:WideString dispid 15;
  end;


// IADsUser : 

 IADsUser = interface(IADs)
   ['{3E37E320-17E2-11CF-ABC4-02608C9E7553}']
   function Get_BadLoginAddress : WideString; safecall;
   function Get_BadLoginCount : Integer; safecall;
   function Get_LastLogin : TDateTime; safecall;
   function Get_LastLogoff : TDateTime; safecall;
   function Get_LastFailedLogin : TDateTime; safecall;
   function Get_PasswordLastChanged : TDateTime; safecall;
   function Get_Description : WideString; safecall;
   procedure Set_Description(const retval:WideString); safecall;
   function Get_Division : WideString; safecall;
   procedure Set_Division(const retval:WideString); safecall;
   function Get_Department : WideString; safecall;
   procedure Set_Department(const retval:WideString); safecall;
   function Get_EmployeeID : WideString; safecall;
   procedure Set_EmployeeID(const retval:WideString); safecall;
   function Get_FullName : WideString; safecall;
   procedure Set_FullName(const retval:WideString); safecall;
   function Get_FirstName : WideString; safecall;
   procedure Set_FirstName(const retval:WideString); safecall;
   function Get_LastName : WideString; safecall;
   procedure Set_LastName(const retval:WideString); safecall;
   function Get_OtherName : WideString; safecall;
   procedure Set_OtherName(const retval:WideString); safecall;
   function Get_NamePrefix : WideString; safecall;
   procedure Set_NamePrefix(const retval:WideString); safecall;
   function Get_NameSuffix : WideString; safecall;
   procedure Set_NameSuffix(const retval:WideString); safecall;
   function Get_Title : WideString; safecall;
   procedure Set_Title(const retval:WideString); safecall;
   function Get_Manager : WideString; safecall;
   procedure Set_Manager(const retval:WideString); safecall;
   function Get_TelephoneHome : OleVariant; safecall;
   procedure Set_TelephoneHome(const retval:OleVariant); safecall;
   function Get_TelephoneMobile : OleVariant; safecall;
   procedure Set_TelephoneMobile(const retval:OleVariant); safecall;
   function Get_TelephoneNumber : OleVariant; safecall;
   procedure Set_TelephoneNumber(const retval:OleVariant); safecall;
   function Get_TelephonePager : OleVariant; safecall;
   procedure Set_TelephonePager(const retval:OleVariant); safecall;
   function Get_FaxNumber : OleVariant; safecall;
   procedure Set_FaxNumber(const retval:OleVariant); safecall;
   function Get_OfficeLocations : OleVariant; safecall;
   procedure Set_OfficeLocations(const retval:OleVariant); safecall;
   function Get_PostalAddresses : OleVariant; safecall;
   procedure Set_PostalAddresses(const retval:OleVariant); safecall;
   function Get_PostalCodes : OleVariant; safecall;
   procedure Set_PostalCodes(const retval:OleVariant); safecall;
   function Get_SeeAlso : OleVariant; safecall;
   procedure Set_SeeAlso(const retval:OleVariant); safecall;
   function Get_AccountDisabled : WordBool; safecall;
   procedure Set_AccountDisabled(const retval:WordBool); safecall;
   function Get_AccountExpirationDate : TDateTime; safecall;
   procedure Set_AccountExpirationDate(const retval:TDateTime); safecall;
   function Get_GraceLoginsAllowed : Integer; safecall;
   procedure Set_GraceLoginsAllowed(const retval:Integer); safecall;
   function Get_GraceLoginsRemaining : Integer; safecall;
   procedure Set_GraceLoginsRemaining(const retval:Integer); safecall;
   function Get_IsAccountLocked : WordBool; safecall;
   procedure Set_IsAccountLocked(const retval:WordBool); safecall;
   function Get_LoginHours : OleVariant; safecall;
   procedure Set_LoginHours(const retval:OleVariant); safecall;
   function Get_LoginWorkstations : OleVariant; safecall;
   procedure Set_LoginWorkstations(const retval:OleVariant); safecall;
   function Get_MaxLogins : Integer; safecall;
   procedure Set_MaxLogins(const retval:Integer); safecall;
   function Get_MaxStorage : Integer; safecall;
   procedure Set_MaxStorage(const retval:Integer); safecall;
   function Get_PasswordExpirationDate : TDateTime; safecall;
   procedure Set_PasswordExpirationDate(const retval:TDateTime); safecall;
   function Get_PasswordMinimumLength : Integer; safecall;
   procedure Set_PasswordMinimumLength(const retval:Integer); safecall;
   function Get_PasswordRequired : WordBool; safecall;
   procedure Set_PasswordRequired(const retval:WordBool); safecall;
   function Get_RequireUniquePassword : WordBool; safecall;
   procedure Set_RequireUniquePassword(const retval:WordBool); safecall;
   function Get_EmailAddress : WideString; safecall;
   procedure Set_EmailAddress(const retval:WideString); safecall;
   function Get_HomeDirectory : WideString; safecall;
   procedure Set_HomeDirectory(const retval:WideString); safecall;
   function Get_Languages : OleVariant; safecall;
   procedure Set_Languages(const retval:OleVariant); safecall;
   function Get_Profile : WideString; safecall;
   procedure Set_Profile(const retval:WideString); safecall;
   function Get_LoginScript : WideString; safecall;
   procedure Set_LoginScript(const retval:WideString); safecall;
   function Get_Picture : OleVariant; safecall;
   procedure Set_Picture(const retval:OleVariant); safecall;
   function Get_HomePage : WideString; safecall;
   procedure Set_HomePage(const retval:WideString); safecall;
    // Groups :  
   function Groups:IADsMembers;safecall;
    // SetPassword :  
   procedure SetPassword(NewPassword:WideString);safecall;
    // ChangePassword :  
   procedure ChangePassword(bstrOldPassword:WideString;bstrNewPassword:WideString);safecall;
    // BadLoginAddress :  
   property BadLoginAddress:WideString read Get_BadLoginAddress;
    // BadLoginCount :  
   property BadLoginCount:Integer read Get_BadLoginCount;
    // LastLogin :  
   property LastLogin:TDateTime read Get_LastLogin;
    // LastLogoff :  
   property LastLogoff:TDateTime read Get_LastLogoff;
    // LastFailedLogin :  
   property LastFailedLogin:TDateTime read Get_LastFailedLogin;
    // PasswordLastChanged :  
   property PasswordLastChanged:TDateTime read Get_PasswordLastChanged;
    // Description :  
   property Description:WideString read Get_Description write Set_Description;
    // Division :  
   property Division:WideString read Get_Division write Set_Division;
    // Department :  
   property Department:WideString read Get_Department write Set_Department;
    // EmployeeID :  
   property EmployeeID:WideString read Get_EmployeeID write Set_EmployeeID;
    // FullName :  
   property FullName:WideString read Get_FullName write Set_FullName;
    // FirstName :  
   property FirstName:WideString read Get_FirstName write Set_FirstName;
    // LastName :  
   property LastName:WideString read Get_LastName write Set_LastName;
    // OtherName :  
   property OtherName:WideString read Get_OtherName write Set_OtherName;
    // NamePrefix :  
   property NamePrefix:WideString read Get_NamePrefix write Set_NamePrefix;
    // NameSuffix :  
   property NameSuffix:WideString read Get_NameSuffix write Set_NameSuffix;
    // Title :  
   property Title:WideString read Get_Title write Set_Title;
    // Manager :  
   property Manager:WideString read Get_Manager write Set_Manager;
    // TelephoneHome :  
   property TelephoneHome:OleVariant read Get_TelephoneHome write Set_TelephoneHome;
    // TelephoneMobile :  
   property TelephoneMobile:OleVariant read Get_TelephoneMobile write Set_TelephoneMobile;
    // TelephoneNumber :  
   property TelephoneNumber:OleVariant read Get_TelephoneNumber write Set_TelephoneNumber;
    // TelephonePager :  
   property TelephonePager:OleVariant read Get_TelephonePager write Set_TelephonePager;
    // FaxNumber :  
   property FaxNumber:OleVariant read Get_FaxNumber write Set_FaxNumber;
    // OfficeLocations :  
   property OfficeLocations:OleVariant read Get_OfficeLocations write Set_OfficeLocations;
    // PostalAddresses :  
   property PostalAddresses:OleVariant read Get_PostalAddresses write Set_PostalAddresses;
    // PostalCodes :  
   property PostalCodes:OleVariant read Get_PostalCodes write Set_PostalCodes;
    // SeeAlso :  
   property SeeAlso:OleVariant read Get_SeeAlso write Set_SeeAlso;
    // AccountDisabled :  
   property AccountDisabled:WordBool read Get_AccountDisabled write Set_AccountDisabled;
    // AccountExpirationDate :  
   property AccountExpirationDate:TDateTime read Get_AccountExpirationDate write Set_AccountExpirationDate;
    // GraceLoginsAllowed :  
   property GraceLoginsAllowed:Integer read Get_GraceLoginsAllowed write Set_GraceLoginsAllowed;
    // GraceLoginsRemaining :  
   property GraceLoginsRemaining:Integer read Get_GraceLoginsRemaining write Set_GraceLoginsRemaining;
    // IsAccountLocked :  
   property IsAccountLocked:WordBool read Get_IsAccountLocked write Set_IsAccountLocked;
    // LoginHours :  
   property LoginHours:OleVariant read Get_LoginHours write Set_LoginHours;
    // LoginWorkstations :  
   property LoginWorkstations:OleVariant read Get_LoginWorkstations write Set_LoginWorkstations;
    // MaxLogins :  
   property MaxLogins:Integer read Get_MaxLogins write Set_MaxLogins;
    // MaxStorage :  
   property MaxStorage:Integer read Get_MaxStorage write Set_MaxStorage;
    // PasswordExpirationDate :  
   property PasswordExpirationDate:TDateTime read Get_PasswordExpirationDate write Set_PasswordExpirationDate;
    // PasswordMinimumLength :  
   property PasswordMinimumLength:Integer read Get_PasswordMinimumLength write Set_PasswordMinimumLength;
    // PasswordRequired :  
   property PasswordRequired:WordBool read Get_PasswordRequired write Set_PasswordRequired;
    // RequireUniquePassword :  
   property RequireUniquePassword:WordBool read Get_RequireUniquePassword write Set_RequireUniquePassword;
    // EmailAddress :  
   property EmailAddress:WideString read Get_EmailAddress write Set_EmailAddress;
    // HomeDirectory :  
   property HomeDirectory:WideString read Get_HomeDirectory write Set_HomeDirectory;
    // Languages :  
   property Languages:OleVariant read Get_Languages write Set_Languages;
    // Profile :  
   property Profile:WideString read Get_Profile write Set_Profile;
    // LoginScript :  
   property LoginScript:WideString read Get_LoginScript write Set_LoginScript;
    // Picture :  
   property Picture:OleVariant read Get_Picture write Set_Picture;
    // HomePage :  
   property HomePage:WideString read Get_HomePage write Set_HomePage;
  end;


// IADsUser : 

 IADsUserDisp = dispinterface
   ['{3E37E320-17E2-11CF-ABC4-02608C9E7553}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetInfo :  
   procedure GetInfo;dispid 8;
    // SetInfo :  
   procedure SetInfo;dispid 9;
    // Get :  
   function Get(bstrName:WideString):OleVariant;dispid 10;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);dispid 11;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;dispid 12;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);dispid 13;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);dispid 14;
    // Groups :  
   function Groups:IADsMembers;dispid 66;
    // SetPassword :  
   procedure SetPassword(NewPassword:WideString);dispid 67;
    // ChangePassword :  
   procedure ChangePassword(bstrOldPassword:WideString;bstrNewPassword:WideString);dispid 68;
    // Name :  
   property Name:WideString  readonly dispid 2;
    // Class :  
   property Class_:WideString  readonly dispid 3;
    // GUID :  
   property GUID:WideString  readonly dispid 4;
    // ADsPath :  
   property ADsPath:WideString  readonly dispid 5;
    // Parent :  
   property Parent:WideString  readonly dispid 6;
    // Schema :  
   property Schema:WideString  readonly dispid 7;
    // BadLoginAddress :  
   property BadLoginAddress:WideString  readonly dispid 53;
    // BadLoginCount :  
   property BadLoginCount:Integer  readonly dispid 54;
    // LastLogin :  
   property LastLogin:TDateTime  readonly dispid 56;
    // LastLogoff :  
   property LastLogoff:TDateTime  readonly dispid 57;
    // LastFailedLogin :  
   property LastFailedLogin:TDateTime  readonly dispid 58;
    // PasswordLastChanged :  
   property PasswordLastChanged:TDateTime  readonly dispid 59;
    // Description :  
   property Description:WideString dispid 15;
    // Division :  
   property Division:WideString dispid 19;
    // Department :  
   property Department:WideString dispid 122;
    // EmployeeID :  
   property EmployeeID:WideString dispid 20;
    // FullName :  
   property FullName:WideString dispid 23;
    // FirstName :  
   property FirstName:WideString dispid 22;
    // LastName :  
   property LastName:WideString dispid 25;
    // OtherName :  
   property OtherName:WideString dispid 27;
    // NamePrefix :  
   property NamePrefix:WideString dispid 114;
    // NameSuffix :  
   property NameSuffix:WideString dispid 115;
    // Title :  
   property Title:WideString dispid 36;
    // Manager :  
   property Manager:WideString dispid 26;
    // TelephoneHome :  
   property TelephoneHome:OleVariant dispid 32;
    // TelephoneMobile :  
   property TelephoneMobile:OleVariant dispid 33;
    // TelephoneNumber :  
   property TelephoneNumber:OleVariant dispid 34;
    // TelephonePager :  
   property TelephonePager:OleVariant dispid 17;
    // FaxNumber :  
   property FaxNumber:OleVariant dispid 16;
    // OfficeLocations :  
   property OfficeLocations:OleVariant dispid 28;
    // PostalAddresses :  
   property PostalAddresses:OleVariant dispid 30;
    // PostalCodes :  
   property PostalCodes:OleVariant dispid 31;
    // SeeAlso :  
   property SeeAlso:OleVariant dispid 117;
    // AccountDisabled :  
   property AccountDisabled:WordBool dispid 37;
    // AccountExpirationDate :  
   property AccountExpirationDate:TDateTime dispid 38;
    // GraceLoginsAllowed :  
   property GraceLoginsAllowed:Integer dispid 41;
    // GraceLoginsRemaining :  
   property GraceLoginsRemaining:Integer dispid 42;
    // IsAccountLocked :  
   property IsAccountLocked:WordBool dispid 43;
    // LoginHours :  
   property LoginHours:OleVariant dispid 45;
    // LoginWorkstations :  
   property LoginWorkstations:OleVariant dispid 46;
    // MaxLogins :  
   property MaxLogins:Integer dispid 47;
    // MaxStorage :  
   property MaxStorage:Integer dispid 48;
    // PasswordExpirationDate :  
   property PasswordExpirationDate:TDateTime dispid 49;
    // PasswordMinimumLength :  
   property PasswordMinimumLength:Integer dispid 50;
    // PasswordRequired :  
   property PasswordRequired:WordBool dispid 51;
    // RequireUniquePassword :  
   property RequireUniquePassword:WordBool dispid 52;
    // EmailAddress :  
   property EmailAddress:WideString dispid 60;
    // HomeDirectory :  
   property HomeDirectory:WideString dispid 61;
    // Languages :  
   property Languages:OleVariant dispid 62;
    // Profile :  
   property Profile:WideString dispid 63;
    // LoginScript :  
   property LoginScript:WideString dispid 64;
    // Picture :  
   property Picture:OleVariant dispid 65;
    // HomePage :  
   property HomePage:WideString dispid 120;
  end;


// IADsPrintQueue : 

 IADsPrintQueue = interface(IADs)
   ['{B15160D0-1226-11CF-A985-00AA006BC149}']
   function Get_PrinterPath : WideString; safecall;
   procedure Set_PrinterPath(const retval:WideString); safecall;
   function Get_Model : WideString; safecall;
   procedure Set_Model(const retval:WideString); safecall;
   function Get_Datatype : WideString; safecall;
   procedure Set_Datatype(const retval:WideString); safecall;
   function Get_PrintProcessor : WideString; safecall;
   procedure Set_PrintProcessor(const retval:WideString); safecall;
   function Get_Description : WideString; safecall;
   procedure Set_Description(const retval:WideString); safecall;
   function Get_Location : WideString; safecall;
   procedure Set_Location(const retval:WideString); safecall;
   function Get_StartTime : TDateTime; safecall;
   procedure Set_StartTime(const retval:TDateTime); safecall;
   function Get_UntilTime : TDateTime; safecall;
   procedure Set_UntilTime(const retval:TDateTime); safecall;
   function Get_DefaultJobPriority : Integer; safecall;
   procedure Set_DefaultJobPriority(const retval:Integer); safecall;
   function Get_Priority : Integer; safecall;
   procedure Set_Priority(const retval:Integer); safecall;
   function Get_BannerPage : WideString; safecall;
   procedure Set_BannerPage(const retval:WideString); safecall;
   function Get_PrintDevices : OleVariant; safecall;
   procedure Set_PrintDevices(const retval:OleVariant); safecall;
   function Get_NetAddresses : OleVariant; safecall;
   procedure Set_NetAddresses(const retval:OleVariant); safecall;
    // PrinterPath :  
   property PrinterPath:WideString read Get_PrinterPath write Set_PrinterPath;
    // Model :  
   property Model:WideString read Get_Model write Set_Model;
    // Datatype :  
   property Datatype:WideString read Get_Datatype write Set_Datatype;
    // PrintProcessor :  
   property PrintProcessor:WideString read Get_PrintProcessor write Set_PrintProcessor;
    // Description :  
   property Description:WideString read Get_Description write Set_Description;
    // Location :  
   property Location:WideString read Get_Location write Set_Location;
    // StartTime :  
   property StartTime:TDateTime read Get_StartTime write Set_StartTime;
    // UntilTime :  
   property UntilTime:TDateTime read Get_UntilTime write Set_UntilTime;
    // DefaultJobPriority :  
   property DefaultJobPriority:Integer read Get_DefaultJobPriority write Set_DefaultJobPriority;
    // Priority :  
   property Priority:Integer read Get_Priority write Set_Priority;
    // BannerPage :  
   property BannerPage:WideString read Get_BannerPage write Set_BannerPage;
    // PrintDevices :  
   property PrintDevices:OleVariant read Get_PrintDevices write Set_PrintDevices;
    // NetAddresses :  
   property NetAddresses:OleVariant read Get_NetAddresses write Set_NetAddresses;
  end;


// IADsPrintQueue : 

 IADsPrintQueueDisp = dispinterface
   ['{B15160D0-1226-11CF-A985-00AA006BC149}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetInfo :  
   procedure GetInfo;dispid 8;
    // SetInfo :  
   procedure SetInfo;dispid 9;
    // Get :  
   function Get(bstrName:WideString):OleVariant;dispid 10;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);dispid 11;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;dispid 12;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);dispid 13;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);dispid 14;
    // Name :  
   property Name:WideString  readonly dispid 2;
    // Class :  
   property Class_:WideString  readonly dispid 3;
    // GUID :  
   property GUID:WideString  readonly dispid 4;
    // ADsPath :  
   property ADsPath:WideString  readonly dispid 5;
    // Parent :  
   property Parent:WideString  readonly dispid 6;
    // Schema :  
   property Schema:WideString  readonly dispid 7;
    // PrinterPath :  
   property PrinterPath:WideString dispid 15;
    // Model :  
   property Model:WideString dispid 16;
    // Datatype :  
   property Datatype:WideString dispid 17;
    // PrintProcessor :  
   property PrintProcessor:WideString dispid 18;
    // Description :  
   property Description:WideString dispid 19;
    // Location :  
   property Location:WideString dispid 20;
    // StartTime :  
   property StartTime:TDateTime dispid 21;
    // UntilTime :  
   property UntilTime:TDateTime dispid 22;
    // DefaultJobPriority :  
   property DefaultJobPriority:Integer dispid 23;
    // Priority :  
   property Priority:Integer dispid 24;
    // BannerPage :  
   property BannerPage:WideString dispid 25;
    // PrintDevices :  
   property PrintDevices:OleVariant dispid 26;
    // NetAddresses :  
   property NetAddresses:OleVariant dispid 27;
  end;


// IADsPrintQueueOperations : 

 IADsPrintQueueOperations = interface(IADs)
   ['{124BE5C0-156E-11CF-A986-00AA006BC149}']
   function Get_Status : Integer; safecall;
    // PrintJobs :  
   function PrintJobs:IADsCollection;safecall;
    // Pause :  
   procedure Pause;safecall;
    // Resume :  
   procedure Resume;safecall;
    // Purge :  
   procedure Purge;safecall;
    // Status :  
   property Status:Integer read Get_Status;
  end;


// IADsPrintQueueOperations : 

 IADsPrintQueueOperationsDisp = dispinterface
   ['{124BE5C0-156E-11CF-A986-00AA006BC149}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetInfo :  
   procedure GetInfo;dispid 8;
    // SetInfo :  
   procedure SetInfo;dispid 9;
    // Get :  
   function Get(bstrName:WideString):OleVariant;dispid 10;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);dispid 11;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;dispid 12;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);dispid 13;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);dispid 14;
    // PrintJobs :  
   function PrintJobs:IADsCollection;dispid 28;
    // Pause :  
   procedure Pause;dispid 29;
    // Resume :  
   procedure Resume;dispid 30;
    // Purge :  
   procedure Purge;dispid 31;
    // Name :  
   property Name:WideString  readonly dispid 2;
    // Class :  
   property Class_:WideString  readonly dispid 3;
    // GUID :  
   property GUID:WideString  readonly dispid 4;
    // ADsPath :  
   property ADsPath:WideString  readonly dispid 5;
    // Parent :  
   property Parent:WideString  readonly dispid 6;
    // Schema :  
   property Schema:WideString  readonly dispid 7;
    // Status :  
   property Status:Integer  readonly dispid 27;
  end;


// IADsPrintJob : 

 IADsPrintJob = interface(IADs)
   ['{32FB6780-1ED0-11CF-A988-00AA006BC149}']
   function Get_HostPrintQueue : WideString; safecall;
   function Get_User : WideString; safecall;
   function Get_UserPath : WideString; safecall;
   function Get_TimeSubmitted : TDateTime; safecall;
   function Get_TotalPages : Integer; safecall;
   function Get_Size : Integer; safecall;
   function Get_Description : WideString; safecall;
   procedure Set_Description(const retval:WideString); safecall;
   function Get_Priority : Integer; safecall;
   procedure Set_Priority(const retval:Integer); safecall;
   function Get_StartTime : TDateTime; safecall;
   procedure Set_StartTime(const retval:TDateTime); safecall;
   function Get_UntilTime : TDateTime; safecall;
   procedure Set_UntilTime(const retval:TDateTime); safecall;
   function Get_Notify : WideString; safecall;
   procedure Set_Notify(const retval:WideString); safecall;
   function Get_NotifyPath : WideString; safecall;
   procedure Set_NotifyPath(const retval:WideString); safecall;
    // HostPrintQueue :  
   property HostPrintQueue:WideString read Get_HostPrintQueue;
    // User :  
   property User:WideString read Get_User;
    // UserPath :  
   property UserPath:WideString read Get_UserPath;
    // TimeSubmitted :  
   property TimeSubmitted:TDateTime read Get_TimeSubmitted;
    // TotalPages :  
   property TotalPages:Integer read Get_TotalPages;
    // Size :  
   property Size:Integer read Get_Size;
    // Description :  
   property Description:WideString read Get_Description write Set_Description;
    // Priority :  
   property Priority:Integer read Get_Priority write Set_Priority;
    // StartTime :  
   property StartTime:TDateTime read Get_StartTime write Set_StartTime;
    // UntilTime :  
   property UntilTime:TDateTime read Get_UntilTime write Set_UntilTime;
    // Notify :  
   property Notify:WideString read Get_Notify write Set_Notify;
    // NotifyPath :  
   property NotifyPath:WideString read Get_NotifyPath write Set_NotifyPath;
  end;


// IADsPrintJob : 

 IADsPrintJobDisp = dispinterface
   ['{32FB6780-1ED0-11CF-A988-00AA006BC149}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetInfo :  
   procedure GetInfo;dispid 8;
    // SetInfo :  
   procedure SetInfo;dispid 9;
    // Get :  
   function Get(bstrName:WideString):OleVariant;dispid 10;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);dispid 11;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;dispid 12;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);dispid 13;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);dispid 14;
    // Name :  
   property Name:WideString  readonly dispid 2;
    // Class :  
   property Class_:WideString  readonly dispid 3;
    // GUID :  
   property GUID:WideString  readonly dispid 4;
    // ADsPath :  
   property ADsPath:WideString  readonly dispid 5;
    // Parent :  
   property Parent:WideString  readonly dispid 6;
    // Schema :  
   property Schema:WideString  readonly dispid 7;
    // HostPrintQueue :  
   property HostPrintQueue:WideString  readonly dispid 15;
    // User :  
   property User:WideString  readonly dispid 16;
    // UserPath :  
   property UserPath:WideString  readonly dispid 17;
    // TimeSubmitted :  
   property TimeSubmitted:TDateTime  readonly dispid 18;
    // TotalPages :  
   property TotalPages:Integer  readonly dispid 19;
    // Size :  
   property Size:Integer  readonly dispid 234;
    // Description :  
   property Description:WideString dispid 20;
    // Priority :  
   property Priority:Integer dispid 21;
    // StartTime :  
   property StartTime:TDateTime dispid 22;
    // UntilTime :  
   property UntilTime:TDateTime dispid 23;
    // Notify :  
   property Notify:WideString dispid 24;
    // NotifyPath :  
   property NotifyPath:WideString dispid 25;
  end;


// IADsPrintJobOperations : 

 IADsPrintJobOperations = interface(IADs)
   ['{9A52DB30-1ECF-11CF-A988-00AA006BC149}']
   function Get_Status : Integer; safecall;
   function Get_TimeElapsed : Integer; safecall;
   function Get_PagesPrinted : Integer; safecall;
   function Get_Position : Integer; safecall;
   procedure Set_Position(const retval:Integer); safecall;
    // Pause :  
   procedure Pause;safecall;
    // Resume :  
   procedure Resume;safecall;
    // Status :  
   property Status:Integer read Get_Status;
    // TimeElapsed :  
   property TimeElapsed:Integer read Get_TimeElapsed;
    // PagesPrinted :  
   property PagesPrinted:Integer read Get_PagesPrinted;
    // Position :  
   property Position:Integer read Get_Position write Set_Position;
  end;


// IADsPrintJobOperations : 

 IADsPrintJobOperationsDisp = dispinterface
   ['{9A52DB30-1ECF-11CF-A988-00AA006BC149}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetInfo :  
   procedure GetInfo;dispid 8;
    // SetInfo :  
   procedure SetInfo;dispid 9;
    // Get :  
   function Get(bstrName:WideString):OleVariant;dispid 10;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);dispid 11;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;dispid 12;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);dispid 13;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);dispid 14;
    // Pause :  
   procedure Pause;dispid 30;
    // Resume :  
   procedure Resume;dispid 31;
    // Name :  
   property Name:WideString  readonly dispid 2;
    // Class :  
   property Class_:WideString  readonly dispid 3;
    // GUID :  
   property GUID:WideString  readonly dispid 4;
    // ADsPath :  
   property ADsPath:WideString  readonly dispid 5;
    // Parent :  
   property Parent:WideString  readonly dispid 6;
    // Schema :  
   property Schema:WideString  readonly dispid 7;
    // Status :  
   property Status:Integer  readonly dispid 26;
    // TimeElapsed :  
   property TimeElapsed:Integer  readonly dispid 27;
    // PagesPrinted :  
   property PagesPrinted:Integer  readonly dispid 28;
    // Position :  
   property Position:Integer dispid 29;
  end;


// IADsService : 

 IADsService = interface(IADs)
   ['{68AF66E0-31CA-11CF-A98A-00AA006BC149}']
   function Get_HostComputer : WideString; safecall;
   procedure Set_HostComputer(const retval:WideString); safecall;
   function Get_DisplayName : WideString; safecall;
   procedure Set_DisplayName(const retval:WideString); safecall;
   function Get_Version : WideString; safecall;
   procedure Set_Version(const retval:WideString); safecall;
   function Get_ServiceType : Integer; safecall;
   procedure Set_ServiceType(const retval:Integer); safecall;
   function Get_StartType : Integer; safecall;
   procedure Set_StartType(const retval:Integer); safecall;
   function Get_Path : WideString; safecall;
   procedure Set_Path(const retval:WideString); safecall;
   function Get_StartupParameters : WideString; safecall;
   procedure Set_StartupParameters(const retval:WideString); safecall;
   function Get_ErrorControl : Integer; safecall;
   procedure Set_ErrorControl(const retval:Integer); safecall;
   function Get_LoadOrderGroup : WideString; safecall;
   procedure Set_LoadOrderGroup(const retval:WideString); safecall;
   function Get_ServiceAccountName : WideString; safecall;
   procedure Set_ServiceAccountName(const retval:WideString); safecall;
   function Get_ServiceAccountPath : WideString; safecall;
   procedure Set_ServiceAccountPath(const retval:WideString); safecall;
   function Get_Dependencies : OleVariant; safecall;
   procedure Set_Dependencies(const retval:OleVariant); safecall;
    // HostComputer :  
   property HostComputer:WideString read Get_HostComputer write Set_HostComputer;
    // DisplayName :  
   property DisplayName:WideString read Get_DisplayName write Set_DisplayName;
    // Version :  
   property Version:WideString read Get_Version write Set_Version;
    // ServiceType :  
   property ServiceType:Integer read Get_ServiceType write Set_ServiceType;
    // StartType :  
   property StartType:Integer read Get_StartType write Set_StartType;
    // Path :  
   property Path:WideString read Get_Path write Set_Path;
    // StartupParameters :  
   property StartupParameters:WideString read Get_StartupParameters write Set_StartupParameters;
    // ErrorControl :  
   property ErrorControl:Integer read Get_ErrorControl write Set_ErrorControl;
    // LoadOrderGroup :  
   property LoadOrderGroup:WideString read Get_LoadOrderGroup write Set_LoadOrderGroup;
    // ServiceAccountName :  
   property ServiceAccountName:WideString read Get_ServiceAccountName write Set_ServiceAccountName;
    // ServiceAccountPath :  
   property ServiceAccountPath:WideString read Get_ServiceAccountPath write Set_ServiceAccountPath;
    // Dependencies :  
   property Dependencies:OleVariant read Get_Dependencies write Set_Dependencies;
  end;


// IADsService : 

 IADsServiceDisp = dispinterface
   ['{68AF66E0-31CA-11CF-A98A-00AA006BC149}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetInfo :  
   procedure GetInfo;dispid 8;
    // SetInfo :  
   procedure SetInfo;dispid 9;
    // Get :  
   function Get(bstrName:WideString):OleVariant;dispid 10;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);dispid 11;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;dispid 12;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);dispid 13;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);dispid 14;
    // Name :  
   property Name:WideString  readonly dispid 2;
    // Class :  
   property Class_:WideString  readonly dispid 3;
    // GUID :  
   property GUID:WideString  readonly dispid 4;
    // ADsPath :  
   property ADsPath:WideString  readonly dispid 5;
    // Parent :  
   property Parent:WideString  readonly dispid 6;
    // Schema :  
   property Schema:WideString  readonly dispid 7;
    // HostComputer :  
   property HostComputer:WideString dispid 15;
    // DisplayName :  
   property DisplayName:WideString dispid 16;
    // Version :  
   property Version:WideString dispid 17;
    // ServiceType :  
   property ServiceType:Integer dispid 18;
    // StartType :  
   property StartType:Integer dispid 19;
    // Path :  
   property Path:WideString dispid 20;
    // StartupParameters :  
   property StartupParameters:WideString dispid 21;
    // ErrorControl :  
   property ErrorControl:Integer dispid 22;
    // LoadOrderGroup :  
   property LoadOrderGroup:WideString dispid 23;
    // ServiceAccountName :  
   property ServiceAccountName:WideString dispid 24;
    // ServiceAccountPath :  
   property ServiceAccountPath:WideString dispid 25;
    // Dependencies :  
   property Dependencies:OleVariant dispid 26;
  end;


// IADsServiceOperations : 

 IADsServiceOperations = interface(IADs)
   ['{5D7B33F0-31CA-11CF-A98A-00AA006BC149}']
   function Get_Status : Integer; safecall;
    // Start :  
   procedure Start;safecall;
    // Stop :  
   procedure Stop;safecall;
    // Pause :  
   procedure Pause;safecall;
    // Continue_ :  
   procedure Continue_;safecall;
    // SetPassword :  
   procedure SetPassword(bstrNewPassword:WideString);safecall;
    // Status :  
   property Status:Integer read Get_Status;
  end;


// IADsServiceOperations : 

 IADsServiceOperationsDisp = dispinterface
   ['{5D7B33F0-31CA-11CF-A98A-00AA006BC149}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetInfo :  
   procedure GetInfo;dispid 8;
    // SetInfo :  
   procedure SetInfo;dispid 9;
    // Get :  
   function Get(bstrName:WideString):OleVariant;dispid 10;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);dispid 11;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;dispid 12;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);dispid 13;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);dispid 14;
    // Start :  
   procedure Start;dispid 28;
    // Stop :  
   procedure Stop;dispid 29;
    // Pause :  
   procedure Pause;dispid 30;
    // Continue_ :  
   procedure Continue_;dispid 31;
    // SetPassword :  
   procedure SetPassword(bstrNewPassword:WideString);dispid 32;
    // Name :  
   property Name:WideString  readonly dispid 2;
    // Class :  
   property Class_:WideString  readonly dispid 3;
    // GUID :  
   property GUID:WideString  readonly dispid 4;
    // ADsPath :  
   property ADsPath:WideString  readonly dispid 5;
    // Parent :  
   property Parent:WideString  readonly dispid 6;
    // Schema :  
   property Schema:WideString  readonly dispid 7;
    // Status :  
   property Status:Integer  readonly dispid 27;
  end;


// IADsFileService : 

 IADsFileService = interface(IADsService)
   ['{A89D1900-31CA-11CF-A98A-00AA006BC149}']
   function Get_Description : WideString; safecall;
   procedure Set_Description(const retval:WideString); safecall;
   function Get_MaxUserCount : Integer; safecall;
   procedure Set_MaxUserCount(const retval:Integer); safecall;
    // Description :  
   property Description:WideString read Get_Description write Set_Description;
    // MaxUserCount :  
   property MaxUserCount:Integer read Get_MaxUserCount write Set_MaxUserCount;
  end;


// IADsFileService : 

 IADsFileServiceDisp = dispinterface
   ['{A89D1900-31CA-11CF-A98A-00AA006BC149}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetInfo :  
   procedure GetInfo;dispid 8;
    // SetInfo :  
   procedure SetInfo;dispid 9;
    // Get :  
   function Get(bstrName:WideString):OleVariant;dispid 10;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);dispid 11;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;dispid 12;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);dispid 13;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);dispid 14;
    // Name :  
   property Name:WideString  readonly dispid 2;
    // Class :  
   property Class_:WideString  readonly dispid 3;
    // GUID :  
   property GUID:WideString  readonly dispid 4;
    // ADsPath :  
   property ADsPath:WideString  readonly dispid 5;
    // Parent :  
   property Parent:WideString  readonly dispid 6;
    // Schema :  
   property Schema:WideString  readonly dispid 7;
    // HostComputer :  
   property HostComputer:WideString dispid 15;
    // DisplayName :  
   property DisplayName:WideString dispid 16;
    // Version :  
   property Version:WideString dispid 17;
    // ServiceType :  
   property ServiceType:Integer dispid 18;
    // StartType :  
   property StartType:Integer dispid 19;
    // Path :  
   property Path:WideString dispid 20;
    // StartupParameters :  
   property StartupParameters:WideString dispid 21;
    // ErrorControl :  
   property ErrorControl:Integer dispid 22;
    // LoadOrderGroup :  
   property LoadOrderGroup:WideString dispid 23;
    // ServiceAccountName :  
   property ServiceAccountName:WideString dispid 24;
    // ServiceAccountPath :  
   property ServiceAccountPath:WideString dispid 25;
    // Dependencies :  
   property Dependencies:OleVariant dispid 26;
    // Description :  
   property Description:WideString dispid 33;
    // MaxUserCount :  
   property MaxUserCount:Integer dispid 34;
  end;


// IADsFileServiceOperations : 

 IADsFileServiceOperations = interface(IADsServiceOperations)
   ['{A02DED10-31CA-11CF-A98A-00AA006BC149}']
    // Sessions :  
   function Sessions:IADsCollection;safecall;
    // Resources :  
   function Resources:IADsCollection;safecall;
  end;


// IADsFileServiceOperations : 

 IADsFileServiceOperationsDisp = dispinterface
   ['{A02DED10-31CA-11CF-A98A-00AA006BC149}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetInfo :  
   procedure GetInfo;dispid 8;
    // SetInfo :  
   procedure SetInfo;dispid 9;
    // Get :  
   function Get(bstrName:WideString):OleVariant;dispid 10;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);dispid 11;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;dispid 12;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);dispid 13;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);dispid 14;
    // Start :  
   procedure Start;dispid 28;
    // Stop :  
   procedure Stop;dispid 29;
    // Pause :  
   procedure Pause;dispid 30;
    // Continue_ :  
   procedure Continue_;dispid 31;
    // SetPassword :  
   procedure SetPassword(bstrNewPassword:WideString);dispid 32;
    // Sessions :  
   function Sessions:IADsCollection;dispid 35;
    // Resources :  
   function Resources:IADsCollection;dispid 36;
    // Name :  
   property Name:WideString  readonly dispid 2;
    // Class :  
   property Class_:WideString  readonly dispid 3;
    // GUID :  
   property GUID:WideString  readonly dispid 4;
    // ADsPath :  
   property ADsPath:WideString  readonly dispid 5;
    // Parent :  
   property Parent:WideString  readonly dispid 6;
    // Schema :  
   property Schema:WideString  readonly dispid 7;
    // Status :  
   property Status:Integer  readonly dispid 27;
  end;


// IADsFileShare : 

 IADsFileShare = interface(IADs)
   ['{EB6DCAF0-4B83-11CF-A995-00AA006BC149}']
   function Get_CurrentUserCount : Integer; safecall;
   function Get_Description : WideString; safecall;
   procedure Set_Description(const retval:WideString); safecall;
   function Get_HostComputer : WideString; safecall;
   procedure Set_HostComputer(const retval:WideString); safecall;
   function Get_Path : WideString; safecall;
   procedure Set_Path(const retval:WideString); safecall;
   function Get_MaxUserCount : Integer; safecall;
   procedure Set_MaxUserCount(const retval:Integer); safecall;
    // CurrentUserCount :  
   property CurrentUserCount:Integer read Get_CurrentUserCount;
    // Description :  
   property Description:WideString read Get_Description write Set_Description;
    // HostComputer :  
   property HostComputer:WideString read Get_HostComputer write Set_HostComputer;
    // Path :  
   property Path:WideString read Get_Path write Set_Path;
    // MaxUserCount :  
   property MaxUserCount:Integer read Get_MaxUserCount write Set_MaxUserCount;
  end;


// IADsFileShare : 

 IADsFileShareDisp = dispinterface
   ['{EB6DCAF0-4B83-11CF-A995-00AA006BC149}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetInfo :  
   procedure GetInfo;dispid 8;
    // SetInfo :  
   procedure SetInfo;dispid 9;
    // Get :  
   function Get(bstrName:WideString):OleVariant;dispid 10;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);dispid 11;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;dispid 12;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);dispid 13;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);dispid 14;
    // Name :  
   property Name:WideString  readonly dispid 2;
    // Class :  
   property Class_:WideString  readonly dispid 3;
    // GUID :  
   property GUID:WideString  readonly dispid 4;
    // ADsPath :  
   property ADsPath:WideString  readonly dispid 5;
    // Parent :  
   property Parent:WideString  readonly dispid 6;
    // Schema :  
   property Schema:WideString  readonly dispid 7;
    // CurrentUserCount :  
   property CurrentUserCount:Integer  readonly dispid 15;
    // Description :  
   property Description:WideString dispid 16;
    // HostComputer :  
   property HostComputer:WideString dispid 17;
    // Path :  
   property Path:WideString dispid 18;
    // MaxUserCount :  
   property MaxUserCount:Integer dispid 19;
  end;


// IADsSession : 

 IADsSession = interface(IADs)
   ['{398B7DA0-4AAB-11CF-AE2C-00AA006EBFB9}']
   function Get_User : WideString; safecall;
   function Get_UserPath : WideString; safecall;
   function Get_Computer : WideString; safecall;
   function Get_ComputerPath : WideString; safecall;
   function Get_ConnectTime : Integer; safecall;
   function Get_IdleTime : Integer; safecall;
    // User :  
   property User:WideString read Get_User;
    // UserPath :  
   property UserPath:WideString read Get_UserPath;
    // Computer :  
   property Computer:WideString read Get_Computer;
    // ComputerPath :  
   property ComputerPath:WideString read Get_ComputerPath;
    // ConnectTime :  
   property ConnectTime:Integer read Get_ConnectTime;
    // IdleTime :  
   property IdleTime:Integer read Get_IdleTime;
  end;


// IADsSession : 

 IADsSessionDisp = dispinterface
   ['{398B7DA0-4AAB-11CF-AE2C-00AA006EBFB9}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetInfo :  
   procedure GetInfo;dispid 8;
    // SetInfo :  
   procedure SetInfo;dispid 9;
    // Get :  
   function Get(bstrName:WideString):OleVariant;dispid 10;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);dispid 11;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;dispid 12;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);dispid 13;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);dispid 14;
    // Name :  
   property Name:WideString  readonly dispid 2;
    // Class :  
   property Class_:WideString  readonly dispid 3;
    // GUID :  
   property GUID:WideString  readonly dispid 4;
    // ADsPath :  
   property ADsPath:WideString  readonly dispid 5;
    // Parent :  
   property Parent:WideString  readonly dispid 6;
    // Schema :  
   property Schema:WideString  readonly dispid 7;
    // User :  
   property User:WideString  readonly dispid 15;
    // UserPath :  
   property UserPath:WideString  readonly dispid 16;
    // Computer :  
   property Computer:WideString  readonly dispid 17;
    // ComputerPath :  
   property ComputerPath:WideString  readonly dispid 18;
    // ConnectTime :  
   property ConnectTime:Integer  readonly dispid 19;
    // IdleTime :  
   property IdleTime:Integer  readonly dispid 20;
  end;


// IADsResource : 

 IADsResource = interface(IADs)
   ['{34A05B20-4AAB-11CF-AE2C-00AA006EBFB9}']
   function Get_User : WideString; safecall;
   function Get_UserPath : WideString; safecall;
   function Get_Path : WideString; safecall;
   function Get_LockCount : Integer; safecall;
    // User :  
   property User:WideString read Get_User;
    // UserPath :  
   property UserPath:WideString read Get_UserPath;
    // Path :  
   property Path:WideString read Get_Path;
    // LockCount :  
   property LockCount:Integer read Get_LockCount;
  end;


// IADsResource : 

 IADsResourceDisp = dispinterface
   ['{34A05B20-4AAB-11CF-AE2C-00AA006EBFB9}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetInfo :  
   procedure GetInfo;dispid 8;
    // SetInfo :  
   procedure SetInfo;dispid 9;
    // Get :  
   function Get(bstrName:WideString):OleVariant;dispid 10;
    // Put :  
   procedure Put(bstrName:WideString;vProp:OleVariant);dispid 11;
    // GetEx :  
   function GetEx(bstrName:WideString):OleVariant;dispid 12;
    // PutEx :  
   procedure PutEx(lnControlCode:Integer;bstrName:WideString;vProp:OleVariant);dispid 13;
    // GetInfoEx :  
   procedure GetInfoEx(vProperties:OleVariant;lnReserved:Integer);dispid 14;
    // Name :  
   property Name:WideString  readonly dispid 2;
    // Class :  
   property Class_:WideString  readonly dispid 3;
    // GUID :  
   property GUID:WideString  readonly dispid 4;
    // ADsPath :  
   property ADsPath:WideString  readonly dispid 5;
    // Parent :  
   property Parent:WideString  readonly dispid 6;
    // Schema :  
   property Schema:WideString  readonly dispid 7;
    // User :  
   property User:WideString  readonly dispid 15;
    // UserPath :  
   property UserPath:WideString  readonly dispid 16;
    // Path :  
   property Path:WideString  readonly dispid 17;
    // LockCount :  
   property LockCount:Integer  readonly dispid 18;
  end;


// IADsOpenDSObject : 

 IADsOpenDSObject = interface(IDispatch)
   ['{DDF2891E-0F9C-11D0-8AD4-00C04FD8D503}']
    // OpenDSObject :  
   function OpenDSObject(lpszDNName:WideString;lpszUserName:WideString;lpszPassword:WideString;lnReserved:Integer):IDispatch;safecall;
  end;


// IADsOpenDSObject : 

 IADsOpenDSObjectDisp = dispinterface
   ['{DDF2891E-0F9C-11D0-8AD4-00C04FD8D503}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // OpenDSObject :  
   function OpenDSObject(lpszDNName:WideString;lpszUserName:WideString;lpszPassword:WideString;lnReserved:Integer):IDispatch;dispid 1;
  end;


// IDirectoryObject : 

 IDirectoryObject = interface(IUnknown)
   ['{E798DE2C-22E4-11D0-84FE-00C04FD8D503}']
    // GetObjectInformation :  
   function GetObjectInformation(out ppObjInfo:P_ads_object_info):HRESULT;stdcall;
    // GetObjectAttributes :  
   function GetObjectAttributes(var pAttributeNames:PWideChar;dwNumberAttributes:LongWord;out ppAttributeEntries:P_ads_attr_info;out pdwNumAttributesReturned:LongWord):HRESULT;stdcall;
    // SetObjectAttributes :  
   function SetObjectAttributes(var pAttributeEntries:_ads_attr_info;dwNumAttributes:LongWord;out pdwNumAttributesModified:LongWord):HRESULT;stdcall;
    // CreateDSObject :  
   function CreateDSObject(pszRDNName:PWideChar;var pAttributeEntries:_ads_attr_info;dwNumAttributes:LongWord;out ppObject:IDispatch):HRESULT;stdcall;
    // DeleteDSObject :  
   function DeleteDSObject(pszRDNName:PWideChar):HRESULT;stdcall;
  end;


// IDirectorySearch : 

 IDirectorySearch = interface(IUnknown)
   ['{109BA8EC-92F0-11D0-A790-00C04FD8D5A8}']
    // SetSearchPreference :  
   function SetSearchPreference(var pSearchPrefs:ads_searchpref_info;dwNumPrefs:LongWord):HRESULT;stdcall;
    // ExecuteSearch :  
   function ExecuteSearch(pszSearchFilter:PWideChar;var pAttributeNames:PWideChar;dwNumberAttributes:LongWord;out phSearchResult:Ppointer):HRESULT;stdcall;
    // AbandonSearch :  
   function AbandonSearch(var phSearchResult:pointer):HRESULT;stdcall;
    // GetFirstRow :  
   function GetFirstRow(var hSearchResult:pointer):HRESULT;stdcall;
    // GetNextRow :  
   function GetNextRow(var hSearchResult:pointer):HRESULT;stdcall;
    // GetPreviousRow :  
   function GetPreviousRow(var hSearchResult:pointer):HRESULT;stdcall;
    // GetNextColumnName :  
   function GetNextColumnName(var hSearchHandle:pointer;out ppszColumnName:PWideChar):HRESULT;stdcall;
    // GetColumn :  
   function GetColumn(var hSearchResult:pointer;szColumnName:PWideChar;out pSearchColumn:ads_search_column):HRESULT;stdcall;
    // FreeColumn :  
   function FreeColumn(var pSearchColumn:ads_search_column):HRESULT;stdcall;
    // CloseSearchHandle :  
   function CloseSearchHandle(var hSearchResult:pointer):HRESULT;stdcall;
  end;


// IDirectorySchemaMgmt : 

 IDirectorySchemaMgmt = interface(IUnknown)
   ['{75DB3B9C-A4D8-11D0-A79C-00C04FD8D5A8}']
    // EnumAttributes :  
   function EnumAttributes(ppszAttrNames:PWideChar;dwNumAttributes:LongWord;ppAttrDefinition:P_ads_attr_def;pdwNumAttributes:LongWord):HRESULT;stdcall;
    // CreateAttributeDefinition :  
   function CreateAttributeDefinition(pszAttributeName:PWideChar;pAttributeDefinition:_ads_attr_def):HRESULT;stdcall;
    // WriteAttributeDefinition :  
   function WriteAttributeDefinition(pszAttributeName:PWideChar;pAttributeDefinition:_ads_attr_def):HRESULT;stdcall;
    // DeleteAttributeDefinition :  
   function DeleteAttributeDefinition(pszAttributeName:PWideChar):HRESULT;stdcall;
    // EnumClasses :  
   function EnumClasses(ppszClassNames:PWideChar;dwNumClasses:LongWord;ppClassDefinition:P_ads_class_def;pdwNumClasses:LongWord):HRESULT;stdcall;
    // WriteClassDefinition :  
   function WriteClassDefinition(pszClassName:PWideChar;pClassDefinition:_ads_class_def):HRESULT;stdcall;
    // CreateClassDefinition :  
   function CreateClassDefinition(pszClassName:PWideChar;pClassDefinition:_ads_class_def):HRESULT;stdcall;
    // DeleteClassDefinition :  
   function DeleteClassDefinition(pszClassName:PWideChar):HRESULT;stdcall;
  end;


// IADsAggregatee : 

 IADsAggregatee = interface(IUnknown)
   ['{1346CE8C-9039-11D0-8528-00C04FD8D503}']
    // ConnectAsAggregatee :  
   function ConnectAsAggregatee(pOuterUnknown:IUnknown):HRESULT;stdcall;
    // DisconnectAsAggregatee :  
   function DisconnectAsAggregatee:HRESULT;stdcall;
    // RelinquishInterface :  
   function RelinquishInterface(riid:GUID):HRESULT;stdcall;
    // RestoreInterface :  
   function RestoreInterface(riid:GUID):HRESULT;stdcall;
  end;


// IADsAggregator : 

 IADsAggregator = interface(IUnknown)
   ['{52DB5FB0-941F-11D0-8529-00C04FD8D503}']
    // ConnectAsAggregator :  
   function ConnectAsAggregator(pAggregatee:IUnknown):HRESULT;stdcall;
    // DisconnectAsAggregator :  
   function DisconnectAsAggregator:HRESULT;stdcall;
  end;


// IADsAccessControlEntry : 

 IADsAccessControlEntry = interface(IDispatch)
   ['{B4F3A14C-9BDD-11D0-852C-00C04FD8D503}']
   function Get_AccessMask : Integer; safecall;
   procedure Set_AccessMask(const retval:Integer); safecall;
   function Get_AceType : Integer; safecall;
   procedure Set_AceType(const retval:Integer); safecall;
   function Get_AceFlags : Integer; safecall;
   procedure Set_AceFlags(const retval:Integer); safecall;
   function Get_Flags : Integer; safecall;
   procedure Set_Flags(const retval:Integer); safecall;
   function Get_ObjectType : WideString; safecall;
   procedure Set_ObjectType(const retval:WideString); safecall;
   function Get_InheritedObjectType : WideString; safecall;
   procedure Set_InheritedObjectType(const retval:WideString); safecall;
   function Get_Trustee : WideString; safecall;
   procedure Set_Trustee(const retval:WideString); safecall;
    // AccessMask :  
   property AccessMask:Integer read Get_AccessMask write Set_AccessMask;
    // AceType :  
   property AceType:Integer read Get_AceType write Set_AceType;
    // AceFlags :  
   property AceFlags:Integer read Get_AceFlags write Set_AceFlags;
    // Flags :  
   property Flags:Integer read Get_Flags write Set_Flags;
    // ObjectType :  
   property ObjectType:WideString read Get_ObjectType write Set_ObjectType;
    // InheritedObjectType :  
   property InheritedObjectType:WideString read Get_InheritedObjectType write Set_InheritedObjectType;
    // Trustee :  
   property Trustee:WideString read Get_Trustee write Set_Trustee;
  end;


// IADsAccessControlEntry : 

 IADsAccessControlEntryDisp = dispinterface
   ['{B4F3A14C-9BDD-11D0-852C-00C04FD8D503}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // AccessMask :  
   property AccessMask:Integer dispid 2;
    // AceType :  
   property AceType:Integer dispid 3;
    // AceFlags :  
   property AceFlags:Integer dispid 4;
    // Flags :  
   property Flags:Integer dispid 5;
    // ObjectType :  
   property ObjectType:WideString dispid 6;
    // InheritedObjectType :  
   property InheritedObjectType:WideString dispid 7;
    // Trustee :  
   property Trustee:WideString dispid 8;
  end;


// IADsAccessControlList : 

 IADsAccessControlList = interface(IDispatch)
   ['{B7EE91CC-9BDD-11D0-852C-00C04FD8D503}']
   function Get_AclRevision : Integer; safecall;
   procedure Set_AclRevision(const retval:Integer); safecall;
   function Get_AceCount : Integer; safecall;
   procedure Set_AceCount(const retval:Integer); safecall;
    // AddAce :  
   procedure AddAce(pAccessControlEntry:IDispatch);safecall;
    // RemoveAce :  
   procedure RemoveAce(pAccessControlEntry:IDispatch);safecall;
    // CopyAccessList :  
   function CopyAccessList:IDispatch;safecall;
   function Get__NewEnum : IUnknown; safecall;
    // AclRevision :  
   property AclRevision:Integer read Get_AclRevision write Set_AclRevision;
    // AceCount :  
   property AceCount:Integer read Get_AceCount write Set_AceCount;
    // _NewEnum :  
   property _NewEnum:IUnknown read Get__NewEnum;
  end;


// IADsAccessControlList : 

 IADsAccessControlListDisp = dispinterface
   ['{B7EE91CC-9BDD-11D0-852C-00C04FD8D503}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // AddAce :  
   procedure AddAce(pAccessControlEntry:IDispatch);dispid 5;
    // RemoveAce :  
   procedure RemoveAce(pAccessControlEntry:IDispatch);dispid 6;
    // CopyAccessList :  
   function CopyAccessList:IDispatch;dispid 7;
    // AclRevision :  
   property AclRevision:Integer dispid 3;
    // AceCount :  
   property AceCount:Integer dispid 4;
    // _NewEnum :  
   property _NewEnum:IUnknown  readonly dispid -4;
  end;


// IADsSecurityDescriptor : 

 IADsSecurityDescriptor = interface(IDispatch)
   ['{B8C787CA-9BDD-11D0-852C-00C04FD8D503}']
   function Get_Revision : Integer; safecall;
   procedure Set_Revision(const retval:Integer); safecall;
   function Get_Control : Integer; safecall;
   procedure Set_Control(const retval:Integer); safecall;
   function Get_Owner : WideString; safecall;
   procedure Set_Owner(const retval:WideString); safecall;
   function Get_OwnerDefaulted : WordBool; safecall;
   procedure Set_OwnerDefaulted(const retval:WordBool); safecall;
   function Get_Group : WideString; safecall;
   procedure Set_Group(const retval:WideString); safecall;
   function Get_GroupDefaulted : WordBool; safecall;
   procedure Set_GroupDefaulted(const retval:WordBool); safecall;
   function Get_DiscretionaryAcl : IDispatch; safecall;
   procedure Set_DiscretionaryAcl(const retval:IDispatch); safecall;
   function Get_DaclDefaulted : WordBool; safecall;
   procedure Set_DaclDefaulted(const retval:WordBool); safecall;
   function Get_SystemAcl : IDispatch; safecall;
   procedure Set_SystemAcl(const retval:IDispatch); safecall;
   function Get_SaclDefaulted : WordBool; safecall;
   procedure Set_SaclDefaulted(const retval:WordBool); safecall;
    // CopySecurityDescriptor :  
   function CopySecurityDescriptor:IDispatch;safecall;
    // Revision :  
   property Revision:Integer read Get_Revision write Set_Revision;
    // Control :  
   property Control:Integer read Get_Control write Set_Control;
    // Owner :  
   property Owner:WideString read Get_Owner write Set_Owner;
    // OwnerDefaulted :  
   property OwnerDefaulted:WordBool read Get_OwnerDefaulted write Set_OwnerDefaulted;
    // Group :  
   property Group:WideString read Get_Group write Set_Group;
    // GroupDefaulted :  
   property GroupDefaulted:WordBool read Get_GroupDefaulted write Set_GroupDefaulted;
    // DiscretionaryAcl :  
   property DiscretionaryAcl:IDispatch read Get_DiscretionaryAcl write Set_DiscretionaryAcl;
    // DaclDefaulted :  
   property DaclDefaulted:WordBool read Get_DaclDefaulted write Set_DaclDefaulted;
    // SystemAcl :  
   property SystemAcl:IDispatch read Get_SystemAcl write Set_SystemAcl;
    // SaclDefaulted :  
   property SaclDefaulted:WordBool read Get_SaclDefaulted write Set_SaclDefaulted;
  end;


// IADsSecurityDescriptor : 

 IADsSecurityDescriptorDisp = dispinterface
   ['{B8C787CA-9BDD-11D0-852C-00C04FD8D503}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // CopySecurityDescriptor :  
   function CopySecurityDescriptor:IDispatch;dispid 12;
    // Revision :  
   property Revision:Integer dispid 2;
    // Control :  
   property Control:Integer dispid 3;
    // Owner :  
   property Owner:WideString dispid 4;
    // OwnerDefaulted :  
   property OwnerDefaulted:WordBool dispid 5;
    // Group :  
   property Group:WideString dispid 6;
    // GroupDefaulted :  
   property GroupDefaulted:WordBool dispid 7;
    // DiscretionaryAcl :  
   property DiscretionaryAcl:IDispatch dispid 8;
    // DaclDefaulted :  
   property DaclDefaulted:WordBool dispid 9;
    // SystemAcl :  
   property SystemAcl:IDispatch dispid 10;
    // SaclDefaulted :  
   property SaclDefaulted:WordBool dispid 11;
  end;


// IADsLargeInteger : 

 IADsLargeInteger = interface(IDispatch)
   ['{9068270B-0939-11D1-8BE1-00C04FD8D503}']
   function Get_HighPart : Integer; safecall;
   procedure Set_HighPart(const retval:Integer); safecall;
   function Get_LowPart : Integer; safecall;
   procedure Set_LowPart(const retval:Integer); safecall;
    // HighPart :  
   property HighPart:Integer read Get_HighPart write Set_HighPart;
    // LowPart :  
   property LowPart:Integer read Get_LowPart write Set_LowPart;
  end;


// IADsLargeInteger : 

 IADsLargeIntegerDisp = dispinterface
   ['{9068270B-0939-11D1-8BE1-00C04FD8D503}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // HighPart :  
   property HighPart:Integer dispid 2;
    // LowPart :  
   property LowPart:Integer dispid 3;
  end;


// IADsNameTranslate : 

 IADsNameTranslate = interface(IDispatch)
   ['{B1B272A3-3625-11D1-A3A4-00C04FB950DC}']
   procedure Set_ChaseReferral(const Param1:Integer); safecall;
    // Init :  
   procedure Init(lnSetType:Integer;bstrADsPath:WideString);safecall;
    // InitEx :  
   procedure InitEx(lnSetType:Integer;bstrADsPath:WideString;bstrUserID:WideString;bstrDomain:WideString;bstrPassword:WideString);safecall;
    // Set_ :  
   procedure Set_(lnSetType:Integer;bstrADsPath:WideString);safecall;
    // Get :  
   function Get(lnFormatType:Integer):WideString;safecall;
    // SetEx :  
   procedure SetEx(lnFormatType:Integer;pVar:OleVariant);safecall;
    // GetEx :  
   function GetEx(lnFormatType:Integer):OleVariant;safecall;
    // ChaseReferral :  
   property ChaseReferral:Integer write Set_ChaseReferral;
  end;


// IADsNameTranslate : 

 IADsNameTranslateDisp = dispinterface
   ['{B1B272A3-3625-11D1-A3A4-00C04FB950DC}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Init :  
   procedure Init(lnSetType:Integer;bstrADsPath:WideString);dispid 2;
    // InitEx :  
   procedure InitEx(lnSetType:Integer;bstrADsPath:WideString;bstrUserID:WideString;bstrDomain:WideString;bstrPassword:WideString);dispid 3;
    // Set_ :  
   procedure Set_(lnSetType:Integer;bstrADsPath:WideString);dispid 4;
    // Get :  
   function Get(lnFormatType:Integer):WideString;dispid 5;
    // SetEx :  
   procedure SetEx(lnFormatType:Integer;pVar:OleVariant);dispid 6;
    // GetEx :  
   function GetEx(lnFormatType:Integer):OleVariant;dispid 7;
    // ChaseReferral :  
   property ChaseReferral:Integer writeonly dispid 1;
  end;


// IADsCaseIgnoreList : 

 IADsCaseIgnoreList = interface(IDispatch)
   ['{7B66B533-4680-11D1-A3B4-00C04FB950DC}']
   function Get_CaseIgnoreList : OleVariant; safecall;
   procedure Set_CaseIgnoreList(const retval:OleVariant); safecall;
    // CaseIgnoreList :  
   property CaseIgnoreList:OleVariant read Get_CaseIgnoreList write Set_CaseIgnoreList;
  end;


// IADsCaseIgnoreList : 

 IADsCaseIgnoreListDisp = dispinterface
   ['{7B66B533-4680-11D1-A3B4-00C04FB950DC}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // CaseIgnoreList :  
   property CaseIgnoreList:OleVariant dispid 2;
  end;


// IADsFaxNumber : 

 IADsFaxNumber = interface(IDispatch)
   ['{A910DEA9-4680-11D1-A3B4-00C04FB950DC}']
   function Get_TelephoneNumber : WideString; safecall;
   procedure Set_TelephoneNumber(const retval:WideString); safecall;
   function Get_Parameters : OleVariant; safecall;
   procedure Set_Parameters(const retval:OleVariant); safecall;
    // TelephoneNumber :  
   property TelephoneNumber:WideString read Get_TelephoneNumber write Set_TelephoneNumber;
    // Parameters :  
   property Parameters:OleVariant read Get_Parameters write Set_Parameters;
  end;


// IADsFaxNumber : 

 IADsFaxNumberDisp = dispinterface
   ['{A910DEA9-4680-11D1-A3B4-00C04FB950DC}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // TelephoneNumber :  
   property TelephoneNumber:WideString dispid 2;
    // Parameters :  
   property Parameters:OleVariant dispid 3;
  end;


// IADsNetAddress : 

 IADsNetAddress = interface(IDispatch)
   ['{B21A50A9-4080-11D1-A3AC-00C04FB950DC}']
   function Get_AddressType : Integer; safecall;
   procedure Set_AddressType(const retval:Integer); safecall;
   function Get_Address : OleVariant; safecall;
   procedure Set_Address(const retval:OleVariant); safecall;
    // AddressType :  
   property AddressType:Integer read Get_AddressType write Set_AddressType;
    // Address :  
   property Address:OleVariant read Get_Address write Set_Address;
  end;


// IADsNetAddress : 

 IADsNetAddressDisp = dispinterface
   ['{B21A50A9-4080-11D1-A3AC-00C04FB950DC}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // AddressType :  
   property AddressType:Integer dispid 2;
    // Address :  
   property Address:OleVariant dispid 3;
  end;


// IADsOctetList : 

 IADsOctetList = interface(IDispatch)
   ['{7B28B80F-4680-11D1-A3B4-00C04FB950DC}']
   function Get_OctetList : OleVariant; safecall;
   procedure Set_OctetList(const retval:OleVariant); safecall;
    // OctetList :  
   property OctetList:OleVariant read Get_OctetList write Set_OctetList;
  end;


// IADsOctetList : 

 IADsOctetListDisp = dispinterface
   ['{7B28B80F-4680-11D1-A3B4-00C04FB950DC}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // OctetList :  
   property OctetList:OleVariant dispid 2;
  end;


// IADsEmail : 

 IADsEmail = interface(IDispatch)
   ['{97AF011A-478E-11D1-A3B4-00C04FB950DC}']
   function Get_Type_ : Integer; safecall;
   procedure Set_Type_(const retval:Integer); safecall;
   function Get_Address : WideString; safecall;
   procedure Set_Address(const retval:WideString); safecall;
    // Type :  
   property Type_:Integer read Get_Type_ write Set_Type_;
    // Address :  
   property Address:WideString read Get_Address write Set_Address;
  end;


// IADsEmail : 

 IADsEmailDisp = dispinterface
   ['{97AF011A-478E-11D1-A3B4-00C04FB950DC}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Type :  
   property Type_:Integer dispid 2;
    // Address :  
   property Address:WideString dispid 3;
  end;


// IADsPath : 

 IADsPath = interface(IDispatch)
   ['{B287FCD5-4080-11D1-A3AC-00C04FB950DC}']
   function Get_Type_ : Integer; safecall;
   procedure Set_Type_(const retval:Integer); safecall;
   function Get_VolumeName : WideString; safecall;
   procedure Set_VolumeName(const retval:WideString); safecall;
   function Get_Path : WideString; safecall;
   procedure Set_Path(const retval:WideString); safecall;
    // Type :  
   property Type_:Integer read Get_Type_ write Set_Type_;
    // VolumeName :  
   property VolumeName:WideString read Get_VolumeName write Set_VolumeName;
    // Path :  
   property Path:WideString read Get_Path write Set_Path;
  end;


// IADsPath : 

 IADsPathDisp = dispinterface
   ['{B287FCD5-4080-11D1-A3AC-00C04FB950DC}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Type :  
   property Type_:Integer dispid 2;
    // VolumeName :  
   property VolumeName:WideString dispid 3;
    // Path :  
   property Path:WideString dispid 4;
  end;


// IADsReplicaPointer : 

 IADsReplicaPointer = interface(IDispatch)
   ['{F60FB803-4080-11D1-A3AC-00C04FB950DC}']
   function Get_ServerName : WideString; safecall;
   procedure Set_ServerName(const retval:WideString); safecall;
   function Get_ReplicaType : Integer; safecall;
   procedure Set_ReplicaType(const retval:Integer); safecall;
   function Get_ReplicaNumber : Integer; safecall;
   procedure Set_ReplicaNumber(const retval:Integer); safecall;
   function Get_Count : Integer; safecall;
   procedure Set_Count(const retval:Integer); safecall;
   function Get_ReplicaAddressHints : OleVariant; safecall;
   procedure Set_ReplicaAddressHints(const retval:OleVariant); safecall;
    // ServerName :  
   property ServerName:WideString read Get_ServerName write Set_ServerName;
    // ReplicaType :  
   property ReplicaType:Integer read Get_ReplicaType write Set_ReplicaType;
    // ReplicaNumber :  
   property ReplicaNumber:Integer read Get_ReplicaNumber write Set_ReplicaNumber;
    // Count :  
   property Count:Integer read Get_Count write Set_Count;
    // ReplicaAddressHints :  
   property ReplicaAddressHints:OleVariant read Get_ReplicaAddressHints write Set_ReplicaAddressHints;
  end;


// IADsReplicaPointer : 

 IADsReplicaPointerDisp = dispinterface
   ['{F60FB803-4080-11D1-A3AC-00C04FB950DC}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // ServerName :  
   property ServerName:WideString dispid 2;
    // ReplicaType :  
   property ReplicaType:Integer dispid 3;
    // ReplicaNumber :  
   property ReplicaNumber:Integer dispid 4;
    // Count :  
   property Count:Integer dispid 5;
    // ReplicaAddressHints :  
   property ReplicaAddressHints:OleVariant dispid 6;
  end;


// IADsAcl : 

 IADsAcl = interface(IDispatch)
   ['{8452D3AB-0869-11D1-A377-00C04FB950DC}']
   function Get_ProtectedAttrName : WideString; safecall;
   procedure Set_ProtectedAttrName(const retval:WideString); safecall;
   function Get_SubjectName : WideString; safecall;
   procedure Set_SubjectName(const retval:WideString); safecall;
   function Get_Privileges : Integer; safecall;
   procedure Set_Privileges(const retval:Integer); safecall;
    // CopyAcl :  
   function CopyAcl:IDispatch;safecall;
    // ProtectedAttrName :  
   property ProtectedAttrName:WideString read Get_ProtectedAttrName write Set_ProtectedAttrName;
    // SubjectName :  
   property SubjectName:WideString read Get_SubjectName write Set_SubjectName;
    // Privileges :  
   property Privileges:Integer read Get_Privileges write Set_Privileges;
  end;


// IADsAcl : 

 IADsAclDisp = dispinterface
   ['{8452D3AB-0869-11D1-A377-00C04FB950DC}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // CopyAcl :  
   function CopyAcl:IDispatch;dispid 5;
    // ProtectedAttrName :  
   property ProtectedAttrName:WideString dispid 2;
    // SubjectName :  
   property SubjectName:WideString dispid 3;
    // Privileges :  
   property Privileges:Integer dispid 4;
  end;


// IADsTimestamp : 

 IADsTimestamp = interface(IDispatch)
   ['{B2F5A901-4080-11D1-A3AC-00C04FB950DC}']
   function Get_WholeSeconds : Integer; safecall;
   procedure Set_WholeSeconds(const retval:Integer); safecall;
   function Get_EventID : Integer; safecall;
   procedure Set_EventID(const retval:Integer); safecall;
    // WholeSeconds :  
   property WholeSeconds:Integer read Get_WholeSeconds write Set_WholeSeconds;
    // EventID :  
   property EventID:Integer read Get_EventID write Set_EventID;
  end;


// IADsTimestamp : 

 IADsTimestampDisp = dispinterface
   ['{B2F5A901-4080-11D1-A3AC-00C04FB950DC}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // WholeSeconds :  
   property WholeSeconds:Integer dispid 2;
    // EventID :  
   property EventID:Integer dispid 3;
  end;


// IADsPostalAddress : 

 IADsPostalAddress = interface(IDispatch)
   ['{7ADECF29-4680-11D1-A3B4-00C04FB950DC}']
   function Get_PostalAddress : OleVariant; safecall;
   procedure Set_PostalAddress(const retval:OleVariant); safecall;
    // PostalAddress :  
   property PostalAddress:OleVariant read Get_PostalAddress write Set_PostalAddress;
  end;


// IADsPostalAddress : 

 IADsPostalAddressDisp = dispinterface
   ['{7ADECF29-4680-11D1-A3B4-00C04FB950DC}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // PostalAddress :  
   property PostalAddress:OleVariant dispid 2;
  end;


// IADsBackLink : 

 IADsBackLink = interface(IDispatch)
   ['{FD1302BD-4080-11D1-A3AC-00C04FB950DC}']
   function Get_RemoteID : Integer; safecall;
   procedure Set_RemoteID(const retval:Integer); safecall;
   function Get_ObjectName : WideString; safecall;
   procedure Set_ObjectName(const retval:WideString); safecall;
    // RemoteID :  
   property RemoteID:Integer read Get_RemoteID write Set_RemoteID;
    // ObjectName :  
   property ObjectName:WideString read Get_ObjectName write Set_ObjectName;
  end;


// IADsBackLink : 

 IADsBackLinkDisp = dispinterface
   ['{FD1302BD-4080-11D1-A3AC-00C04FB950DC}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // RemoteID :  
   property RemoteID:Integer dispid 2;
    // ObjectName :  
   property ObjectName:WideString dispid 3;
  end;


// IADsTypedName : 

 IADsTypedName = interface(IDispatch)
   ['{B371A349-4080-11D1-A3AC-00C04FB950DC}']
   function Get_ObjectName : WideString; safecall;
   procedure Set_ObjectName(const retval:WideString); safecall;
   function Get_Level : Integer; safecall;
   procedure Set_Level(const retval:Integer); safecall;
   function Get_Interval : Integer; safecall;
   procedure Set_Interval(const retval:Integer); safecall;
    // ObjectName :  
   property ObjectName:WideString read Get_ObjectName write Set_ObjectName;
    // Level :  
   property Level:Integer read Get_Level write Set_Level;
    // Interval :  
   property Interval:Integer read Get_Interval write Set_Interval;
  end;


// IADsTypedName : 

 IADsTypedNameDisp = dispinterface
   ['{B371A349-4080-11D1-A3AC-00C04FB950DC}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // ObjectName :  
   property ObjectName:WideString dispid 2;
    // Level :  
   property Level:Integer dispid 3;
    // Interval :  
   property Interval:Integer dispid 4;
  end;


// IADsHold : 

 IADsHold = interface(IDispatch)
   ['{B3EB3B37-4080-11D1-A3AC-00C04FB950DC}']
   function Get_ObjectName : WideString; safecall;
   procedure Set_ObjectName(const retval:WideString); safecall;
   function Get_Amount : Integer; safecall;
   procedure Set_Amount(const retval:Integer); safecall;
    // ObjectName :  
   property ObjectName:WideString read Get_ObjectName write Set_ObjectName;
    // Amount :  
   property Amount:Integer read Get_Amount write Set_Amount;
  end;


// IADsHold : 

 IADsHoldDisp = dispinterface
   ['{B3EB3B37-4080-11D1-A3AC-00C04FB950DC}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // ObjectName :  
   property ObjectName:WideString dispid 2;
    // Amount :  
   property Amount:Integer dispid 3;
  end;


// IADsObjectOptions : 

 IADsObjectOptions = interface(IDispatch)
   ['{46F14FDA-232B-11D1-A808-00C04FD8D5A8}']
    // GetOption :  
   function GetOption(lnOption:Integer):OleVariant;safecall;
    // SetOption :  
   procedure SetOption(lnOption:Integer;vValue:OleVariant);safecall;
  end;


// IADsObjectOptions : 

 IADsObjectOptionsDisp = dispinterface
   ['{46F14FDA-232B-11D1-A808-00C04FD8D5A8}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetOption :  
   function GetOption(lnOption:Integer):OleVariant;dispid 2;
    // SetOption :  
   procedure SetOption(lnOption:Integer;vValue:OleVariant);dispid 3;
  end;


// IADsPathname : 

 IADsPathname = interface(IDispatch)
   ['{D592AED4-F420-11D0-A36E-00C04FB950DC}']
    // Set_ :  
   procedure Set_(bstrADsPath:WideString;lnSetType:Integer);safecall;
    // SetDisplayType :  
   procedure SetDisplayType(lnDisplayType:Integer);safecall;
    // Retrieve :  
   function Retrieve(lnFormatType:Integer):WideString;safecall;
    // GetNumElements :  
   function GetNumElements:Integer;safecall;
    // GetElement :  
   function GetElement(lnElementIndex:Integer):WideString;safecall;
    // AddLeafElement :  
   procedure AddLeafElement(bstrLeafElement:WideString);safecall;
    // RemoveLeafElement :  
   procedure RemoveLeafElement;safecall;
    // CopyPath :  
   function CopyPath:IDispatch;safecall;
    // GetEscapedElement :  
   function GetEscapedElement(lnReserved:Integer;bstrInStr:WideString):WideString;safecall;
   function Get_EscapedMode : Integer; safecall;
   procedure Set_EscapedMode(const retval:Integer); safecall;
    // EscapedMode :  
   property EscapedMode:Integer read Get_EscapedMode write Set_EscapedMode;
  end;


// IADsPathname : 

 IADsPathnameDisp = dispinterface
   ['{D592AED4-F420-11D0-A36E-00C04FB950DC}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // Set_ :  
   procedure Set_(bstrADsPath:WideString;lnSetType:Integer);dispid 2;
    // SetDisplayType :  
   procedure SetDisplayType(lnDisplayType:Integer);dispid 3;
    // Retrieve :  
   function Retrieve(lnFormatType:Integer):WideString;dispid 4;
    // GetNumElements :  
   function GetNumElements:Integer;dispid 5;
    // GetElement :  
   function GetElement(lnElementIndex:Integer):WideString;dispid 6;
    // AddLeafElement :  
   procedure AddLeafElement(bstrLeafElement:WideString);dispid 7;
    // RemoveLeafElement :  
   procedure RemoveLeafElement;dispid 8;
    // CopyPath :  
   function CopyPath:IDispatch;dispid 9;
    // GetEscapedElement :  
   function GetEscapedElement(lnReserved:Integer;bstrInStr:WideString):WideString;dispid 10;
    // EscapedMode :  
   property EscapedMode:Integer dispid 11;
  end;


// IADsADSystemInfo : 

 IADsADSystemInfo = interface(IDispatch)
   ['{5BB11929-AFD1-11D2-9CB9-0000F87A369E}']
   function Get_UserName : WideString; safecall;
   function Get_ComputerName : WideString; safecall;
   function Get_SiteName : WideString; safecall;
   function Get_DomainShortName : WideString; safecall;
   function Get_DomainDNSName : WideString; safecall;
   function Get_ForestDNSName : WideString; safecall;
   function Get_PDCRoleOwner : WideString; safecall;
   function Get_SchemaRoleOwner : WideString; safecall;
   function Get_IsNativeMode : WordBool; safecall;
    // GetAnyDCName :  
   function GetAnyDCName:WideString;safecall;
    // GetDCSiteName :  
   function GetDCSiteName(szServer:WideString):WideString;safecall;
    // RefreshSchemaCache :  
   procedure RefreshSchemaCache;safecall;
    // GetTrees :  
   function GetTrees:OleVariant;safecall;
    // UserName :  
   property UserName:WideString read Get_UserName;
    // ComputerName :  
   property ComputerName:WideString read Get_ComputerName;
    // SiteName :  
   property SiteName:WideString read Get_SiteName;
    // DomainShortName :  
   property DomainShortName:WideString read Get_DomainShortName;
    // DomainDNSName :  
   property DomainDNSName:WideString read Get_DomainDNSName;
    // ForestDNSName :  
   property ForestDNSName:WideString read Get_ForestDNSName;
    // PDCRoleOwner :  
   property PDCRoleOwner:WideString read Get_PDCRoleOwner;
    // SchemaRoleOwner :  
   property SchemaRoleOwner:WideString read Get_SchemaRoleOwner;
    // IsNativeMode :  
   property IsNativeMode:WordBool read Get_IsNativeMode;
  end;


// IADsADSystemInfo : 

 IADsADSystemInfoDisp = dispinterface
   ['{5BB11929-AFD1-11D2-9CB9-0000F87A369E}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetAnyDCName :  
   function GetAnyDCName:WideString;dispid 11;
    // GetDCSiteName :  
   function GetDCSiteName(szServer:WideString):WideString;dispid 12;
    // RefreshSchemaCache :  
   procedure RefreshSchemaCache;dispid 13;
    // GetTrees :  
   function GetTrees:OleVariant;dispid 14;
    // UserName :  
   property UserName:WideString  readonly dispid 2;
    // ComputerName :  
   property ComputerName:WideString  readonly dispid 3;
    // SiteName :  
   property SiteName:WideString  readonly dispid 4;
    // DomainShortName :  
   property DomainShortName:WideString  readonly dispid 5;
    // DomainDNSName :  
   property DomainDNSName:WideString  readonly dispid 6;
    // ForestDNSName :  
   property ForestDNSName:WideString  readonly dispid 7;
    // PDCRoleOwner :  
   property PDCRoleOwner:WideString  readonly dispid 8;
    // SchemaRoleOwner :  
   property SchemaRoleOwner:WideString  readonly dispid 9;
    // IsNativeMode :  
   property IsNativeMode:WordBool  readonly dispid 10;
  end;


// IADsWinNTSystemInfo : 

 IADsWinNTSystemInfo = interface(IDispatch)
   ['{6C6D65DC-AFD1-11D2-9CB9-0000F87A369E}']
   function Get_UserName : WideString; safecall;
   function Get_ComputerName : WideString; safecall;
   function Get_DomainName : WideString; safecall;
   function Get_PDC : WideString; safecall;
    // UserName :  
   property UserName:WideString read Get_UserName;
    // ComputerName :  
   property ComputerName:WideString read Get_ComputerName;
    // DomainName :  
   property DomainName:WideString read Get_DomainName;
    // PDC :  
   property PDC:WideString read Get_PDC;
  end;


// IADsWinNTSystemInfo : 

 IADsWinNTSystemInfoDisp = dispinterface
   ['{6C6D65DC-AFD1-11D2-9CB9-0000F87A369E}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // UserName :  
   property UserName:WideString  readonly dispid 2;
    // ComputerName :  
   property ComputerName:WideString  readonly dispid 3;
    // DomainName :  
   property DomainName:WideString  readonly dispid 4;
    // PDC :  
   property PDC:WideString  readonly dispid 5;
  end;


// IADsDNWithBinary : 

 IADsDNWithBinary = interface(IDispatch)
   ['{7E99C0A2-F935-11D2-BA96-00C04FB6D0D1}']
   function Get_BinaryValue : OleVariant; safecall;
   procedure Set_BinaryValue(const retval:OleVariant); safecall;
   function Get_DNString : WideString; safecall;
   procedure Set_DNString(const retval:WideString); safecall;
    // BinaryValue :  
   property BinaryValue:OleVariant read Get_BinaryValue write Set_BinaryValue;
    // DNString :  
   property DNString:WideString read Get_DNString write Set_DNString;
  end;


// IADsDNWithBinary : 

 IADsDNWithBinaryDisp = dispinterface
   ['{7E99C0A2-F935-11D2-BA96-00C04FB6D0D1}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // BinaryValue :  
   property BinaryValue:OleVariant dispid 2;
    // DNString :  
   property DNString:WideString dispid 3;
  end;


// IADsDNWithString : 

 IADsDNWithString = interface(IDispatch)
   ['{370DF02E-F934-11D2-BA96-00C04FB6D0D1}']
   function Get_StringValue : WideString; safecall;
   procedure Set_StringValue(const retval:WideString); safecall;
   function Get_DNString : WideString; safecall;
   procedure Set_DNString(const retval:WideString); safecall;
    // StringValue :  
   property StringValue:WideString read Get_StringValue write Set_StringValue;
    // DNString :  
   property DNString:WideString read Get_DNString write Set_DNString;
  end;


// IADsDNWithString : 

 IADsDNWithStringDisp = dispinterface
   ['{370DF02E-F934-11D2-BA96-00C04FB6D0D1}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // StringValue :  
   property StringValue:WideString dispid 2;
    // DNString :  
   property DNString:WideString dispid 3;
  end;


// IADsSecurityUtility : 

 IADsSecurityUtility = interface(IDispatch)
   ['{A63251B2-5F21-474B-AB52-4A8EFAD10895}']
    // GetSecurityDescriptor :  
   function GetSecurityDescriptor(varPath:OleVariant;lPathFormat:Integer;lFormat:Integer):OleVariant;safecall;
    // SetSecurityDescriptor :  
   procedure SetSecurityDescriptor(varPath:OleVariant;lPathFormat:Integer;varData:OleVariant;lDataFormat:Integer);safecall;
    // ConvertSecurityDescriptor :  
   function ConvertSecurityDescriptor(varSD:OleVariant;lDataFormat:Integer;lOutFormat:Integer):OleVariant;safecall;
   function Get_SecurityMask : Integer; safecall;
   procedure Set_SecurityMask(const retval:Integer); safecall;
    // SecurityMask :  
   property SecurityMask:Integer read Get_SecurityMask write Set_SecurityMask;
  end;


// IADsSecurityUtility : 

 IADsSecurityUtilityDisp = dispinterface
   ['{A63251B2-5F21-474B-AB52-4A8EFAD10895}']
    // QueryInterface :  
   procedure QueryInterface(var riid:{!! GUID !!} OleVariant;out ppvObj:{!! Ppointer !!} OleVariant);dispid 1610612736;
    // AddRef :  
   function AddRef:LongWord;dispid 1610612737;
    // Release :  
   function Release:LongWord;dispid 1610612738;
    // GetTypeInfoCount :  
   procedure GetTypeInfoCount(out pctinfo:UInt);dispid 1610678272;
    // GetTypeInfo :  
   procedure GetTypeInfo(itinfo:UInt;lcid:LongWord;out pptinfo:{!! Ppointer !!} OleVariant);dispid 1610678273;
    // GetIDsOfNames :  
   procedure GetIDsOfNames(var riid:{!! GUID !!} OleVariant;var rgszNames:{!! PShortInt !!} OleVariant;cNames:UInt;lcid:LongWord;out rgdispid:Integer);dispid 1610678274;
    // Invoke :  
   procedure Invoke(dispidMember:Integer;var riid:{!! GUID !!} OleVariant;lcid:LongWord;wFlags:Word;var pdispparams:{!! DISPPARAMS !!} OleVariant;out pvarResult:OleVariant;out pexcepinfo:{!! EXCEPINFO !!} OleVariant;out puArgErr:UInt);dispid 1610678275;
    // GetSecurityDescriptor :  
   function GetSecurityDescriptor(varPath:OleVariant;lPathFormat:Integer;lFormat:Integer):OleVariant;dispid 2;
    // SetSecurityDescriptor :  
   procedure SetSecurityDescriptor(varPath:OleVariant;lPathFormat:Integer;varData:OleVariant;lDataFormat:Integer);dispid 3;
    // ConvertSecurityDescriptor :  
   function ConvertSecurityDescriptor(varSD:OleVariant;lDataFormat:Integer;lOutFormat:Integer):OleVariant;dispid 4;
    // SecurityMask :  
   property SecurityMask:Integer dispid 5;
  end;

//CoClasses
  CoPropertyEntry = Class
  Public
    Class Function Create: IADsPropertyEntry;
    Class Function CreateRemote(const MachineName: string): IADsPropertyEntry;
  end;

  CoPropertyValue = Class
  Public
    Class Function Create: IADsPropertyValue;
    Class Function CreateRemote(const MachineName: string): IADsPropertyValue;
  end;

  CoAccessControlEntry = Class
  Public
    Class Function Create: IADsAccessControlEntry;
    Class Function CreateRemote(const MachineName: string): IADsAccessControlEntry;
  end;

  CoAccessControlList = Class
  Public
    Class Function Create: IADsAccessControlList;
    Class Function CreateRemote(const MachineName: string): IADsAccessControlList;
  end;

  CoSecurityDescriptor = Class
  Public
    Class Function Create: IADsSecurityDescriptor;
    Class Function CreateRemote(const MachineName: string): IADsSecurityDescriptor;
  end;

  CoLargeInteger = Class
  Public
    Class Function Create: IADsLargeInteger;
    Class Function CreateRemote(const MachineName: string): IADsLargeInteger;
  end;

  CoNameTranslate = Class
  Public
    Class Function Create: IADsNameTranslate;
    Class Function CreateRemote(const MachineName: string): IADsNameTranslate;
  end;

  CoCaseIgnoreList = Class
  Public
    Class Function Create: IADsCaseIgnoreList;
    Class Function CreateRemote(const MachineName: string): IADsCaseIgnoreList;
  end;

  CoFaxNumber = Class
  Public
    Class Function Create: IADsFaxNumber;
    Class Function CreateRemote(const MachineName: string): IADsFaxNumber;
  end;

  CoNetAddress = Class
  Public
    Class Function Create: IADsNetAddress;
    Class Function CreateRemote(const MachineName: string): IADsNetAddress;
  end;

  CoOctetList = Class
  Public
    Class Function Create: IADsOctetList;
    Class Function CreateRemote(const MachineName: string): IADsOctetList;
  end;

  CoEmail = Class
  Public
    Class Function Create: IADsEmail;
    Class Function CreateRemote(const MachineName: string): IADsEmail;
  end;

  CoPath = Class
  Public
    Class Function Create: IADsPath;
    Class Function CreateRemote(const MachineName: string): IADsPath;
  end;

  CoReplicaPointer = Class
  Public
    Class Function Create: IADsReplicaPointer;
    Class Function CreateRemote(const MachineName: string): IADsReplicaPointer;
  end;

  CoTimestamp = Class
  Public
    Class Function Create: IADsTimestamp;
    Class Function CreateRemote(const MachineName: string): IADsTimestamp;
  end;

  CoPostalAddress = Class
  Public
    Class Function Create: IADsPostalAddress;
    Class Function CreateRemote(const MachineName: string): IADsPostalAddress;
  end;

  CoBackLink = Class
  Public
    Class Function Create: IADsBackLink;
    Class Function CreateRemote(const MachineName: string): IADsBackLink;
  end;

  CoTypedName = Class
  Public
    Class Function Create: IADsTypedName;
    Class Function CreateRemote(const MachineName: string): IADsTypedName;
  end;

  CoHold = Class
  Public
    Class Function Create: IADsHold;
    Class Function CreateRemote(const MachineName: string): IADsHold;
  end;

  CoPathname = Class
  Public
    Class Function Create: IADsPathname;
    Class Function CreateRemote(const MachineName: string): IADsPathname;
  end;

  CoADSystemInfo = Class
  Public
    Class Function Create: IADsADSystemInfo;
    Class Function CreateRemote(const MachineName: string): IADsADSystemInfo;
  end;

  CoWinNTSystemInfo = Class
  Public
    Class Function Create: IADsWinNTSystemInfo;
    Class Function CreateRemote(const MachineName: string): IADsWinNTSystemInfo;
  end;

  CoDNWithBinary = Class
  Public
    Class Function Create: IADsDNWithBinary;
    Class Function CreateRemote(const MachineName: string): IADsDNWithBinary;
  end;

  CoDNWithString = Class
  Public
    Class Function Create: IADsDNWithString;
    Class Function CreateRemote(const MachineName: string): IADsDNWithString;
  end;

  CoADsSecurityUtility = Class
  Public
    Class Function Create: IADsSecurityUtility;
    Class Function CreateRemote(const MachineName: string): IADsSecurityUtility;
  end;

implementation

uses comobj;

Class Function CoPropertyEntry.Create: IADsPropertyEntry;
begin
  Result := CreateComObject(CLASS_PropertyEntry) as IADsPropertyEntry;
end;

Class Function CoPropertyEntry.CreateRemote(const MachineName: string): IADsPropertyEntry;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_PropertyEntry) as IADsPropertyEntry;
end;

Class Function CoPropertyValue.Create: IADsPropertyValue;
begin
  Result := CreateComObject(CLASS_PropertyValue) as IADsPropertyValue;
end;

Class Function CoPropertyValue.CreateRemote(const MachineName: string): IADsPropertyValue;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_PropertyValue) as IADsPropertyValue;
end;

Class Function CoAccessControlEntry.Create: IADsAccessControlEntry;
begin
  Result := CreateComObject(CLASS_AccessControlEntry) as IADsAccessControlEntry;
end;

Class Function CoAccessControlEntry.CreateRemote(const MachineName: string): IADsAccessControlEntry;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_AccessControlEntry) as IADsAccessControlEntry;
end;

Class Function CoAccessControlList.Create: IADsAccessControlList;
begin
  Result := CreateComObject(CLASS_AccessControlList) as IADsAccessControlList;
end;

Class Function CoAccessControlList.CreateRemote(const MachineName: string): IADsAccessControlList;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_AccessControlList) as IADsAccessControlList;
end;

Class Function CoSecurityDescriptor.Create: IADsSecurityDescriptor;
begin
  Result := CreateComObject(CLASS_SecurityDescriptor) as IADsSecurityDescriptor;
end;

Class Function CoSecurityDescriptor.CreateRemote(const MachineName: string): IADsSecurityDescriptor;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SecurityDescriptor) as IADsSecurityDescriptor;
end;

Class Function CoLargeInteger.Create: IADsLargeInteger;
begin
  Result := CreateComObject(CLASS_LargeInteger) as IADsLargeInteger;
end;

Class Function CoLargeInteger.CreateRemote(const MachineName: string): IADsLargeInteger;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_LargeInteger) as IADsLargeInteger;
end;

Class Function CoNameTranslate.Create: IADsNameTranslate;
begin
  Result := CreateComObject(CLASS_NameTranslate) as IADsNameTranslate;
end;

Class Function CoNameTranslate.CreateRemote(const MachineName: string): IADsNameTranslate;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_NameTranslate) as IADsNameTranslate;
end;

Class Function CoCaseIgnoreList.Create: IADsCaseIgnoreList;
begin
  Result := CreateComObject(CLASS_CaseIgnoreList) as IADsCaseIgnoreList;
end;

Class Function CoCaseIgnoreList.CreateRemote(const MachineName: string): IADsCaseIgnoreList;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_CaseIgnoreList) as IADsCaseIgnoreList;
end;

Class Function CoFaxNumber.Create: IADsFaxNumber;
begin
  Result := CreateComObject(CLASS_FaxNumber) as IADsFaxNumber;
end;

Class Function CoFaxNumber.CreateRemote(const MachineName: string): IADsFaxNumber;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_FaxNumber) as IADsFaxNumber;
end;

Class Function CoNetAddress.Create: IADsNetAddress;
begin
  Result := CreateComObject(CLASS_NetAddress) as IADsNetAddress;
end;

Class Function CoNetAddress.CreateRemote(const MachineName: string): IADsNetAddress;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_NetAddress) as IADsNetAddress;
end;

Class Function CoOctetList.Create: IADsOctetList;
begin
  Result := CreateComObject(CLASS_OctetList) as IADsOctetList;
end;

Class Function CoOctetList.CreateRemote(const MachineName: string): IADsOctetList;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_OctetList) as IADsOctetList;
end;

Class Function CoEmail.Create: IADsEmail;
begin
  Result := CreateComObject(CLASS_Email) as IADsEmail;
end;

Class Function CoEmail.CreateRemote(const MachineName: string): IADsEmail;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_Email) as IADsEmail;
end;

Class Function CoPath.Create: IADsPath;
begin
  Result := CreateComObject(CLASS_Path) as IADsPath;
end;

Class Function CoPath.CreateRemote(const MachineName: string): IADsPath;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_Path) as IADsPath;
end;

Class Function CoReplicaPointer.Create: IADsReplicaPointer;
begin
  Result := CreateComObject(CLASS_ReplicaPointer) as IADsReplicaPointer;
end;

Class Function CoReplicaPointer.CreateRemote(const MachineName: string): IADsReplicaPointer;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_ReplicaPointer) as IADsReplicaPointer;
end;

Class Function CoTimestamp.Create: IADsTimestamp;
begin
  Result := CreateComObject(CLASS_Timestamp) as IADsTimestamp;
end;

Class Function CoTimestamp.CreateRemote(const MachineName: string): IADsTimestamp;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_Timestamp) as IADsTimestamp;
end;

Class Function CoPostalAddress.Create: IADsPostalAddress;
begin
  Result := CreateComObject(CLASS_PostalAddress) as IADsPostalAddress;
end;

Class Function CoPostalAddress.CreateRemote(const MachineName: string): IADsPostalAddress;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_PostalAddress) as IADsPostalAddress;
end;

Class Function CoBackLink.Create: IADsBackLink;
begin
  Result := CreateComObject(CLASS_BackLink) as IADsBackLink;
end;

Class Function CoBackLink.CreateRemote(const MachineName: string): IADsBackLink;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_BackLink) as IADsBackLink;
end;

Class Function CoTypedName.Create: IADsTypedName;
begin
  Result := CreateComObject(CLASS_TypedName) as IADsTypedName;
end;

Class Function CoTypedName.CreateRemote(const MachineName: string): IADsTypedName;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_TypedName) as IADsTypedName;
end;

Class Function CoHold.Create: IADsHold;
begin
  Result := CreateComObject(CLASS_Hold) as IADsHold;
end;

Class Function CoHold.CreateRemote(const MachineName: string): IADsHold;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_Hold) as IADsHold;
end;

Class Function CoPathname.Create: IADsPathname;
begin
  Result := CreateComObject(CLASS_Pathname) as IADsPathname;
end;

Class Function CoPathname.CreateRemote(const MachineName: string): IADsPathname;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_Pathname) as IADsPathname;
end;

Class Function CoADSystemInfo.Create: IADsADSystemInfo;
begin
  Result := CreateComObject(CLASS_ADSystemInfo) as IADsADSystemInfo;
end;

Class Function CoADSystemInfo.CreateRemote(const MachineName: string): IADsADSystemInfo;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_ADSystemInfo) as IADsADSystemInfo;
end;

Class Function CoWinNTSystemInfo.Create: IADsWinNTSystemInfo;
begin
  Result := CreateComObject(CLASS_WinNTSystemInfo) as IADsWinNTSystemInfo;
end;

Class Function CoWinNTSystemInfo.CreateRemote(const MachineName: string): IADsWinNTSystemInfo;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_WinNTSystemInfo) as IADsWinNTSystemInfo;
end;

Class Function CoDNWithBinary.Create: IADsDNWithBinary;
begin
  Result := CreateComObject(CLASS_DNWithBinary) as IADsDNWithBinary;
end;

Class Function CoDNWithBinary.CreateRemote(const MachineName: string): IADsDNWithBinary;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_DNWithBinary) as IADsDNWithBinary;
end;

Class Function CoDNWithString.Create: IADsDNWithString;
begin
  Result := CreateComObject(CLASS_DNWithString) as IADsDNWithString;
end;

Class Function CoDNWithString.CreateRemote(const MachineName: string): IADsDNWithString;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_DNWithString) as IADsDNWithString;
end;

Class Function CoADsSecurityUtility.Create: IADsSecurityUtility;
begin
  Result := CreateComObject(CLASS_ADsSecurityUtility) as IADsSecurityUtility;
end;

Class Function CoADsSecurityUtility.CreateRemote(const MachineName: string): IADsSecurityUtility;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_ADsSecurityUtility) as IADsSecurityUtility;
end;

end.
