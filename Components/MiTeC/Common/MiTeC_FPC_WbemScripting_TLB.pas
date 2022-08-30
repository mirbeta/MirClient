{*******************************************************}
{               MiTeC Common Routines                   }
{                  WMI interface                        }
{                                                       }
{                                                       }
{        Copyright (c) 1997-2017 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

Unit MiTeC_FPC_WbemScripting_TLB;

//  Imported WbemScripting on 4.4.2017 19:13:07 from C:\Windows\System32\wbem\wbemdisp.tlb

{$H+}

interface

// Dependency: stdole v2 (stdole2.pas)
//  Warning: 'GUID' not automatable in ISWbemServicesdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemServicesdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemServicesdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISWbemServicesdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISWbemServicesdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISWbemServicesdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISWbemServicesdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISWbemServicesdisp.Invoke
//  Warning: 'GUID' not automatable in ISWbemObjectdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemObjectdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemObjectdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISWbemObjectdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISWbemObjectdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISWbemObjectdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISWbemObjectdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISWbemObjectdisp.Invoke
//  Warning: renamed property 'Class' in ISWbemObjectPath to 'Class_'
//  Warning: 'GUID' not automatable in ISWbemObjectPathdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemObjectPathdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemObjectPathdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISWbemObjectPathdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISWbemObjectPathdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISWbemObjectPathdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISWbemObjectPathdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISWbemObjectPathdisp.Invoke
//  Warning: 'GUID' not automatable in ISWbemNamedValueSetdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemNamedValueSetdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemNamedValueSetdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISWbemNamedValueSetdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISWbemNamedValueSetdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISWbemNamedValueSetdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISWbemNamedValueSetdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISWbemNamedValueSetdisp.Invoke
//  Warning: 'GUID' not automatable in ISWbemNamedValuedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemNamedValuedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemNamedValuedisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISWbemNamedValuedisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISWbemNamedValuedisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISWbemNamedValuedisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISWbemNamedValuedisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISWbemNamedValuedisp.Invoke
//  Warning: 'POleVariant' not automatable in ISWbemNamedValue.Value
//  Warning: 'GUID' not automatable in ISWbemSecuritydisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemSecuritydisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemSecuritydisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISWbemSecuritydisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISWbemSecuritydisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISWbemSecuritydisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISWbemSecuritydisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISWbemSecuritydisp.Invoke
//  Warning: 'GUID' not automatable in ISWbemPrivilegeSetdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemPrivilegeSetdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemPrivilegeSetdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISWbemPrivilegeSetdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISWbemPrivilegeSetdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISWbemPrivilegeSetdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISWbemPrivilegeSetdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISWbemPrivilegeSetdisp.Invoke
//  Warning: 'GUID' not automatable in ISWbemPrivilegedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemPrivilegedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemPrivilegedisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISWbemPrivilegedisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISWbemPrivilegedisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISWbemPrivilegedisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISWbemPrivilegedisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISWbemPrivilegedisp.Invoke
//  Warning: 'GUID' not automatable in ISWbemObjectSetdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemObjectSetdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemObjectSetdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISWbemObjectSetdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISWbemObjectSetdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISWbemObjectSetdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISWbemObjectSetdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISWbemObjectSetdisp.Invoke
//  Warning: 'GUID' not automatable in ISWbemQualifierSetdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemQualifierSetdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemQualifierSetdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISWbemQualifierSetdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISWbemQualifierSetdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISWbemQualifierSetdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISWbemQualifierSetdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISWbemQualifierSetdisp.Invoke
//  Warning: 'GUID' not automatable in ISWbemQualifierdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemQualifierdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemQualifierdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISWbemQualifierdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISWbemQualifierdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISWbemQualifierdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISWbemQualifierdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISWbemQualifierdisp.Invoke
//  Warning: 'POleVariant' not automatable in ISWbemQualifier.Value
//  Warning: 'GUID' not automatable in ISWbemPropertySetdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemPropertySetdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemPropertySetdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISWbemPropertySetdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISWbemPropertySetdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISWbemPropertySetdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISWbemPropertySetdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISWbemPropertySetdisp.Invoke
//  Warning: 'GUID' not automatable in ISWbemPropertydisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemPropertydisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemPropertydisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISWbemPropertydisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISWbemPropertydisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISWbemPropertydisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISWbemPropertydisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISWbemPropertydisp.Invoke
//  Warning: 'POleVariant' not automatable in ISWbemProperty.Value
//  Warning: 'GUID' not automatable in ISWbemMethodSetdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemMethodSetdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemMethodSetdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISWbemMethodSetdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISWbemMethodSetdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISWbemMethodSetdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISWbemMethodSetdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISWbemMethodSetdisp.Invoke
//  Warning: 'GUID' not automatable in ISWbemMethoddisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemMethoddisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemMethoddisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISWbemMethoddisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISWbemMethoddisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISWbemMethoddisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISWbemMethoddisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISWbemMethoddisp.Invoke
//  Warning: 'GUID' not automatable in ISWbemEventSourcedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemEventSourcedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemEventSourcedisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISWbemEventSourcedisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISWbemEventSourcedisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISWbemEventSourcedisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISWbemEventSourcedisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISWbemEventSourcedisp.Invoke
//  Warning: 'GUID' not automatable in ISWbemLocatordisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemLocatordisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemLocatordisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISWbemLocatordisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISWbemLocatordisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISWbemLocatordisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISWbemLocatordisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISWbemLocatordisp.Invoke
//  Warning: 'GUID' not automatable in ISWbemLastErrordisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemLastErrordisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemLastErrordisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISWbemLastErrordisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISWbemLastErrordisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISWbemLastErrordisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISWbemLastErrordisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISWbemLastErrordisp.Invoke
//  Warning: 'GUID' not automatable in ISWbemSinkdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemSinkdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemSinkdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISWbemSinkdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISWbemSinkdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISWbemSinkdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISWbemSinkdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISWbemSinkdisp.Invoke
//  Warning: 'GUID' not automatable in ISWbemServicesExdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemServicesExdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemServicesExdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISWbemServicesExdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISWbemServicesExdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISWbemServicesExdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISWbemServicesExdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISWbemServicesExdisp.Invoke
//  Warning: 'GUID' not automatable in ISWbemObjectExdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemObjectExdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemObjectExdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISWbemObjectExdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISWbemObjectExdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISWbemObjectExdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISWbemObjectExdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISWbemObjectExdisp.Invoke
//  Warning: 'GUID' not automatable in ISWbemDateTimedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemDateTimedisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemDateTimedisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISWbemDateTimedisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISWbemDateTimedisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISWbemDateTimedisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISWbemDateTimedisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISWbemDateTimedisp.Invoke
//  Warning: 'GUID' not automatable in ISWbemRefresherdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemRefresherdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemRefresherdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISWbemRefresherdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISWbemRefresherdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISWbemRefresherdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISWbemRefresherdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISWbemRefresherdisp.Invoke
//  Warning: renamed property 'Object' in ISWbemRefreshableItem to 'Object_'
//  Warning: 'GUID' not automatable in ISWbemRefreshableItemdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemRefreshableItemdisp.QueryInterface
//  Warning: 'Ppointer' not automatable in ISWbemRefreshableItemdisp.GetTypeInfo
//  Warning: 'GUID' not automatable in ISWbemRefreshableItemdisp.GetIDsOfNames
//  Warning: 'PShortInt' not automatable in ISWbemRefreshableItemdisp.GetIDsOfNames
//  Warning: 'GUID' not automatable in ISWbemRefreshableItemdisp.Invoke
//  Warning: 'DISPPARAMS' not automatable in ISWbemRefreshableItemdisp.Invoke
//  Warning: 'EXCEPINFO' not automatable in ISWbemRefreshableItemdisp.Invoke
Uses
  Windows,ActiveX,Classes,Variants,stdole2,EventSink;
Const
  WbemScriptingMajorVersion = 1;
  WbemScriptingMinorVersion = 2;
  WbemScriptingLCID = 0;
  LIBID_WbemScripting : TGUID = '{565783C6-CB41-11D1-8B02-00600806D9B6}';

  IID_ISWbemServices : TGUID = '{76A6415C-CB41-11D1-8B02-00600806D9B6}';
  IID_ISWbemObject : TGUID = '{76A6415A-CB41-11D1-8B02-00600806D9B6}';
  IID_ISWbemObjectPath : TGUID = '{5791BC27-CE9C-11D1-97BF-0000F81E849C}';
  IID_ISWbemNamedValueSet : TGUID = '{CF2376EA-CE8C-11D1-8B05-00600806D9B6}';
  IID_ISWbemNamedValue : TGUID = '{76A64164-CB41-11D1-8B02-00600806D9B6}';
  IID_ISWbemSecurity : TGUID = '{B54D66E6-2287-11D2-8B33-00600806D9B6}';
  IID_ISWbemPrivilegeSet : TGUID = '{26EE67BF-5804-11D2-8B4A-00600806D9B6}';
  IID_ISWbemPrivilege : TGUID = '{26EE67BD-5804-11D2-8B4A-00600806D9B6}';
  IID_ISWbemObjectSet : TGUID = '{76A6415F-CB41-11D1-8B02-00600806D9B6}';
  IID_ISWbemQualifierSet : TGUID = '{9B16ED16-D3DF-11D1-8B08-00600806D9B6}';
  IID_ISWbemQualifier : TGUID = '{79B05932-D3B7-11D1-8B06-00600806D9B6}';
  IID_ISWbemPropertySet : TGUID = '{DEA0A7B2-D4BA-11D1-8B09-00600806D9B6}';
  IID_ISWbemProperty : TGUID = '{1A388F98-D4BA-11D1-8B09-00600806D9B6}';
  IID_ISWbemMethodSet : TGUID = '{C93BA292-D955-11D1-8B09-00600806D9B6}';
  IID_ISWbemMethod : TGUID = '{422E8E90-D955-11D1-8B09-00600806D9B6}';
  IID_ISWbemEventSource : TGUID = '{27D54D92-0EBE-11D2-8B22-00600806D9B6}';
  IID_ISWbemLocator : TGUID = '{76A6415B-CB41-11D1-8B02-00600806D9B6}';
  IID_ISWbemLastError : TGUID = '{D962DB84-D4BB-11D1-8B09-00600806D9B6}';
  IID_ISWbemSinkEvents : TGUID = '{75718CA0-F029-11D1-A1AC-00C04FB6C223}';
  IID_ISWbemSink : TGUID = '{75718C9F-F029-11D1-A1AC-00C04FB6C223}';
  IID_ISWbemServicesEx : TGUID = '{D2F68443-85DC-427E-91D8-366554CC754C}';
  IID_ISWbemObjectEx : TGUID = '{269AD56A-8A67-4129-BC8C-0506DCFE9880}';
  IID_ISWbemDateTime : TGUID = '{5E97458A-CF77-11D3-B38F-00105A1F473A}';
  IID_ISWbemRefresher : TGUID = '{14D8250E-D9C2-11D3-B38F-00105A1F473A}';
  IID_ISWbemRefreshableItem : TGUID = '{5AD4BF92-DAAB-11D3-B38F-00105A1F473A}';
  CLASS_SWbemLocator : TGUID = '{76A64158-CB41-11D1-8B02-00600806D9B6}';
  CLASS_SWbemNamedValueSet : TGUID = '{9AED384E-CE8B-11D1-8B05-00600806D9B6}';
  CLASS_SWbemObjectPath : TGUID = '{5791BC26-CE9C-11D1-97BF-0000F81E849C}';
  CLASS_SWbemLastError : TGUID = '{C2FEEEAC-CFCD-11D1-8B05-00600806D9B6}';
  CLASS_SWbemSink : TGUID = '{75718C9A-F029-11D1-A1AC-00C04FB6C223}';
  CLASS_SWbemDateTime : TGUID = '{47DFBE54-CF76-11D3-B38F-00105A1F473A}';
  CLASS_SWbemRefresher : TGUID = '{D269BF5C-D9C1-11D3-B38F-00105A1F473A}';
  CLASS_SWbemServices : TGUID = '{04B83D63-21AE-11D2-8B33-00600806D9B6}';
  CLASS_SWbemServicesEx : TGUID = '{62E522DC-8CF3-40A8-8B2E-37D595651E40}';
  CLASS_SWbemObject : TGUID = '{04B83D62-21AE-11D2-8B33-00600806D9B6}';
  CLASS_SWbemObjectEx : TGUID = '{D6BDAFB2-9435-491F-BB87-6AA0F0BC31A2}';
  CLASS_SWbemObjectSet : TGUID = '{04B83D61-21AE-11D2-8B33-00600806D9B6}';
  CLASS_SWbemNamedValue : TGUID = '{04B83D60-21AE-11D2-8B33-00600806D9B6}';
  CLASS_SWbemQualifier : TGUID = '{04B83D5F-21AE-11D2-8B33-00600806D9B6}';
  CLASS_SWbemQualifierSet : TGUID = '{04B83D5E-21AE-11D2-8B33-00600806D9B6}';
  CLASS_SWbemProperty : TGUID = '{04B83D5D-21AE-11D2-8B33-00600806D9B6}';
  CLASS_SWbemPropertySet : TGUID = '{04B83D5C-21AE-11D2-8B33-00600806D9B6}';
  CLASS_SWbemMethod : TGUID = '{04B83D5B-21AE-11D2-8B33-00600806D9B6}';
  CLASS_SWbemMethodSet : TGUID = '{04B83D5A-21AE-11D2-8B33-00600806D9B6}';
  CLASS_SWbemEventSource : TGUID = '{04B83D58-21AE-11D2-8B33-00600806D9B6}';
  CLASS_SWbemSecurity : TGUID = '{B54D66E9-2287-11D2-8B33-00600806D9B6}';
  CLASS_SWbemPrivilege : TGUID = '{26EE67BC-5804-11D2-8B4A-00600806D9B6}';
  CLASS_SWbemPrivilegeSet : TGUID = '{26EE67BE-5804-11D2-8B4A-00600806D9B6}';
  CLASS_SWbemRefreshableItem : TGUID = '{8C6854BC-DE4B-11D3-B390-00105A1F473A}';

//Enums

Type
  WbemImpersonationLevelEnum =LongWord;
Const
  wbemImpersonationLevelAnonymous = $0000000000000001;
  wbemImpersonationLevelIdentify = $0000000000000002;
  wbemImpersonationLevelImpersonate = $0000000000000003;
  wbemImpersonationLevelDelegate = $0000000000000004;
Type
  WbemAuthenticationLevelEnum =LongWord;
Const
  wbemAuthenticationLevelDefault = $0000000000000000;
  wbemAuthenticationLevelNone = $0000000000000001;
  wbemAuthenticationLevelConnect = $0000000000000002;
  wbemAuthenticationLevelCall = $0000000000000003;
  wbemAuthenticationLevelPkt = $0000000000000004;
  wbemAuthenticationLevelPktIntegrity = $0000000000000005;
  wbemAuthenticationLevelPktPrivacy = $0000000000000006;
Type
  WbemPrivilegeEnum =LongWord;
Const
  wbemPrivilegeCreateToken = $0000000000000001;
  wbemPrivilegePrimaryToken = $0000000000000002;
  wbemPrivilegeLockMemory = $0000000000000003;
  wbemPrivilegeIncreaseQuota = $0000000000000004;
  wbemPrivilegeMachineAccount = $0000000000000005;
  wbemPrivilegeTcb = $0000000000000006;
  wbemPrivilegeSecurity = $0000000000000007;
  wbemPrivilegeTakeOwnership = $0000000000000008;
  wbemPrivilegeLoadDriver = $0000000000000009;
  wbemPrivilegeSystemProfile = $000000000000000A;
  wbemPrivilegeSystemtime = $000000000000000B;
  wbemPrivilegeProfileSingleProcess = $000000000000000C;
  wbemPrivilegeIncreaseBasePriority = $000000000000000D;
  wbemPrivilegeCreatePagefile = $000000000000000E;
  wbemPrivilegeCreatePermanent = $000000000000000F;
  wbemPrivilegeBackup = $0000000000000010;
  wbemPrivilegeRestore = $0000000000000011;
  wbemPrivilegeShutdown = $0000000000000012;
  wbemPrivilegeDebug = $0000000000000013;
  wbemPrivilegeAudit = $0000000000000014;
  wbemPrivilegeSystemEnvironment = $0000000000000015;
  wbemPrivilegeChangeNotify = $0000000000000016;
  wbemPrivilegeRemoteShutdown = $0000000000000017;
  wbemPrivilegeUndock = $0000000000000018;
  wbemPrivilegeSyncAgent = $0000000000000019;
  wbemPrivilegeEnableDelegation = $000000000000001A;
  wbemPrivilegeManageVolume = $000000000000001B;
Type
  WbemCimtypeEnum =LongWord;
Const
  wbemCimtypeSint8 = $0000000000000010;
  wbemCimtypeUint8 = $0000000000000011;
  wbemCimtypeSint16 = $0000000000000002;
  wbemCimtypeUint16 = $0000000000000012;
  wbemCimtypeSint32 = $0000000000000003;
  wbemCimtypeUint32 = $0000000000000013;
  wbemCimtypeSint64 = $0000000000000014;
  wbemCimtypeUint64 = $0000000000000015;
  wbemCimtypeReal32 = $0000000000000004;
  wbemCimtypeReal64 = $0000000000000005;
  wbemCimtypeBoolean = $000000000000000B;
  wbemCimtypeString = $0000000000000008;
  wbemCimtypeDatetime = $0000000000000065;
  wbemCimtypeReference = $0000000000000066;
  wbemCimtypeChar16 = $0000000000000067;
  wbemCimtypeObject = $000000000000000D;
Type
  WbemErrorEnum =LongWord;
Const
  wbemNoErr = $0000000000000000;
  wbemErrFailed = $0000000080041001;
  wbemErrNotFound = $0000000080041002;
  wbemErrAccessDenied = $0000000080041003;
  wbemErrProviderFailure = $0000000080041004;
  wbemErrTypeMismatch = $0000000080041005;
  wbemErrOutOfMemory = $0000000080041006;
  wbemErrInvalidContext = $0000000080041007;
  wbemErrInvalidParameter = $0000000080041008;
  wbemErrNotAvailable = $0000000080041009;
  wbemErrCriticalError = $000000008004100A;
  wbemErrInvalidStream = $000000008004100B;
  wbemErrNotSupported = $000000008004100C;
  wbemErrInvalidSuperclass = $000000008004100D;
  wbemErrInvalidNamespace = $000000008004100E;
  wbemErrInvalidObject = $000000008004100F;
  wbemErrInvalidClass = $0000000080041010;
  wbemErrProviderNotFound = $0000000080041011;
  wbemErrInvalidProviderRegistration = $0000000080041012;
  wbemErrProviderLoadFailure = $0000000080041013;
  wbemErrInitializationFailure = $0000000080041014;
  wbemErrTransportFailure = $0000000080041015;
  wbemErrInvalidOperation = $0000000080041016;
  wbemErrInvalidQuery = $0000000080041017;
  wbemErrInvalidQueryType = $0000000080041018;
  wbemErrAlreadyExists = $0000000080041019;
  wbemErrOverrideNotAllowed = $000000008004101A;
  wbemErrPropagatedQualifier = $000000008004101B;
  wbemErrPropagatedProperty = $000000008004101C;
  wbemErrUnexpected = $000000008004101D;
  wbemErrIllegalOperation = $000000008004101E;
  wbemErrCannotBeKey = $000000008004101F;
  wbemErrIncompleteClass = $0000000080041020;
  wbemErrInvalidSyntax = $0000000080041021;
  wbemErrNondecoratedObject = $0000000080041022;
  wbemErrReadOnly = $0000000080041023;
  wbemErrProviderNotCapable = $0000000080041024;
  wbemErrClassHasChildren = $0000000080041025;
  wbemErrClassHasInstances = $0000000080041026;
  wbemErrQueryNotImplemented = $0000000080041027;
  wbemErrIllegalNull = $0000000080041028;
  wbemErrInvalidQualifierType = $0000000080041029;
  wbemErrInvalidPropertyType = $000000008004102A;
  wbemErrValueOutOfRange = $000000008004102B;
  wbemErrCannotBeSingleton = $000000008004102C;
  wbemErrInvalidCimType = $000000008004102D;
  wbemErrInvalidMethod = $000000008004102E;
  wbemErrInvalidMethodParameters = $000000008004102F;
  wbemErrSystemProperty = $0000000080041030;
  wbemErrInvalidProperty = $0000000080041031;
  wbemErrCallCancelled = $0000000080041032;
  wbemErrShuttingDown = $0000000080041033;
  wbemErrPropagatedMethod = $0000000080041034;
  wbemErrUnsupportedParameter = $0000000080041035;
  wbemErrMissingParameter = $0000000080041036;
  wbemErrInvalidParameterId = $0000000080041037;
  wbemErrNonConsecutiveParameterIds = $0000000080041038;
  wbemErrParameterIdOnRetval = $0000000080041039;
  wbemErrInvalidObjectPath = $000000008004103A;
  wbemErrOutOfDiskSpace = $000000008004103B;
  wbemErrBufferTooSmall = $000000008004103C;
  wbemErrUnsupportedPutExtension = $000000008004103D;
  wbemErrUnknownObjectType = $000000008004103E;
  wbemErrUnknownPacketType = $000000008004103F;
  wbemErrMarshalVersionMismatch = $0000000080041040;
  wbemErrMarshalInvalidSignature = $0000000080041041;
  wbemErrInvalidQualifier = $0000000080041042;
  wbemErrInvalidDuplicateParameter = $0000000080041043;
  wbemErrTooMuchData = $0000000080041044;
  wbemErrServerTooBusy = $0000000080041045;
  wbemErrInvalidFlavor = $0000000080041046;
  wbemErrCircularReference = $0000000080041047;
  wbemErrUnsupportedClassUpdate = $0000000080041048;
  wbemErrCannotChangeKeyInheritance = $0000000080041049;
  wbemErrCannotChangeIndexInheritance = $0000000080041050;
  wbemErrTooManyProperties = $0000000080041051;
  wbemErrUpdateTypeMismatch = $0000000080041052;
  wbemErrUpdateOverrideNotAllowed = $0000000080041053;
  wbemErrUpdatePropagatedMethod = $0000000080041054;
  wbemErrMethodNotImplemented = $0000000080041055;
  wbemErrMethodDisabled = $0000000080041056;
  wbemErrRefresherBusy = $0000000080041057;
  wbemErrUnparsableQuery = $0000000080041058;
  wbemErrNotEventClass = $0000000080041059;
  wbemErrMissingGroupWithin = $000000008004105A;
  wbemErrMissingAggregationList = $000000008004105B;
  wbemErrPropertyNotAnObject = $000000008004105C;
  wbemErrAggregatingByObject = $000000008004105D;
  wbemErrUninterpretableProviderQuery = $000000008004105F;
  wbemErrBackupRestoreWinmgmtRunning = $0000000080041060;
  wbemErrQueueOverflow = $0000000080041061;
  wbemErrPrivilegeNotHeld = $0000000080041062;
  wbemErrInvalidOperator = $0000000080041063;
  wbemErrLocalCredentials = $0000000080041064;
  wbemErrCannotBeAbstract = $0000000080041065;
  wbemErrAmendedObject = $0000000080041066;
  wbemErrClientTooSlow = $0000000080041067;
  wbemErrNullSecurityDescriptor = $0000000080041068;
  wbemErrTimeout = $0000000080041069;
  wbemErrInvalidAssociation = $000000008004106A;
  wbemErrAmbiguousOperation = $000000008004106B;
  wbemErrQuotaViolation = $000000008004106C;
  wbemErrTransactionConflict = $000000008004106D;
  wbemErrForcedRollback = $000000008004106E;
  wbemErrUnsupportedLocale = $000000008004106F;
  wbemErrHandleOutOfDate = $0000000080041070;
  wbemErrConnectionFailed = $0000000080041071;
  wbemErrInvalidHandleRequest = $0000000080041072;
  wbemErrPropertyNameTooWide = $0000000080041073;
  wbemErrClassNameTooWide = $0000000080041074;
  wbemErrMethodNameTooWide = $0000000080041075;
  wbemErrQualifierNameTooWide = $0000000080041076;
  wbemErrRerunCommand = $0000000080041077;
  wbemErrDatabaseVerMismatch = $0000000080041078;
  wbemErrVetoPut = $0000000080041079;
  wbemErrVetoDelete = $000000008004107A;
  wbemErrInvalidLocale = $0000000080041080;
  wbemErrProviderSuspended = $0000000080041081;
  wbemErrSynchronizationRequired = $0000000080041082;
  wbemErrNoSchema = $0000000080041083;
  wbemErrProviderAlreadyRegistered = $0000000080041084;
  wbemErrProviderNotRegistered = $0000000080041085;
  wbemErrFatalTransportError = $0000000080041086;
  wbemErrEncryptedConnectionRequired = $0000000080041087;
  wbemErrRegistrationTooBroad = $0000000080042001;
  wbemErrRegistrationTooPrecise = $0000000080042002;
  wbemErrTimedout = $0000000080043001;
  wbemErrResetToDefault = $0000000080043002;
Type
  WbemObjectTextFormatEnum =LongWord;
Const
  wbemObjectTextFormatCIMDTD20 = $0000000000000001;
  wbemObjectTextFormatWMIDTD20 = $0000000000000002;
Type
  WbemChangeFlagEnum =LongWord;
Const
  wbemChangeFlagCreateOrUpdate = $0000000000000000;
  wbemChangeFlagUpdateOnly = $0000000000000001;
  wbemChangeFlagCreateOnly = $0000000000000002;
  wbemChangeFlagUpdateCompatible = $0000000000000000;
  wbemChangeFlagUpdateSafeMode = $0000000000000020;
  wbemChangeFlagUpdateForceMode = $0000000000000040;
  wbemChangeFlagStrongValidation = $0000000000000080;
  wbemChangeFlagAdvisory = $0000000000010000;
Type
  WbemFlagEnum =LongWord;
Const
  wbemFlagReturnImmediately = $0000000000000010;
  wbemFlagReturnWhenComplete = $0000000000000000;
  wbemFlagBidirectional = $0000000000000000;
  wbemFlagForwardOnly = $0000000000000020;
  wbemFlagNoErrorObject = $0000000000000040;
  wbemFlagReturnErrorObject = $0000000000000000;
  wbemFlagSendStatus = $0000000000000080;
  wbemFlagDontSendStatus = $0000000000000000;
  wbemFlagEnsureLocatable = $0000000000000100;
  wbemFlagDirectRead = $0000000000000200;
  wbemFlagSendOnlySelected = $0000000000000000;
  wbemFlagUseAmendedQualifiers = $0000000000020000;
  wbemFlagGetDefault = $0000000000000000;
  wbemFlagSpawnInstance = $0000000000000001;
  wbemFlagUseCurrentTime = $0000000000000001;
Type
  WbemQueryFlagEnum =LongWord;
Const
  wbemQueryFlagDeep = $0000000000000000;
  wbemQueryFlagShallow = $0000000000000001;
  wbemQueryFlagPrototype = $0000000000000002;
Type
  WbemTextFlagEnum =LongWord;
Const
  wbemTextFlagNoFlavors = $0000000000000001;
Type
  WbemTimeout =LongWord;
Const
  wbemTimeoutInfinite = $00000000FFFFFFFF;
Type
  WbemComparisonFlagEnum =LongWord;
Const
  wbemComparisonFlagIncludeAll = $0000000000000000;
  wbemComparisonFlagIgnoreQualifiers = $0000000000000001;
  wbemComparisonFlagIgnoreObjectSource = $0000000000000002;
  wbemComparisonFlagIgnoreDefaultValues = $0000000000000004;
  wbemComparisonFlagIgnoreClass = $0000000000000008;
  wbemComparisonFlagIgnoreCase = $0000000000000010;
  wbemComparisonFlagIgnoreFlavor = $0000000000000020;
Type
  WbemConnectOptionsEnum =LongWord;
Const
  wbemConnectFlagUseMaxWait = $0000000000000080;
//Forward declarations

Type
 ISWbemServices = interface;
 ISWbemServicesDisp = dispinterface;
 ISWbemObject = interface;
 ISWbemObjectDisp = dispinterface;
 ISWbemObjectPath = interface;
 ISWbemObjectPathDisp = dispinterface;
 ISWbemNamedValueSet = interface;
 ISWbemNamedValueSetDisp = dispinterface;
 ISWbemNamedValue = interface;
 ISWbemNamedValueDisp = dispinterface;
 ISWbemSecurity = interface;
 ISWbemSecurityDisp = dispinterface;
 ISWbemPrivilegeSet = interface;
 ISWbemPrivilegeSetDisp = dispinterface;
 ISWbemPrivilege = interface;
 ISWbemPrivilegeDisp = dispinterface;
 ISWbemObjectSet = interface;
 ISWbemObjectSetDisp = dispinterface;
 ISWbemQualifierSet = interface;
 ISWbemQualifierSetDisp = dispinterface;
 ISWbemQualifier = interface;
 ISWbemQualifierDisp = dispinterface;
 ISWbemPropertySet = interface;
 ISWbemPropertySetDisp = dispinterface;
 ISWbemProperty = interface;
 ISWbemPropertyDisp = dispinterface;
 ISWbemMethodSet = interface;
 ISWbemMethodSetDisp = dispinterface;
 ISWbemMethod = interface;
 ISWbemMethodDisp = dispinterface;
 ISWbemEventSource = interface;
 ISWbemEventSourceDisp = dispinterface;
 ISWbemLocator = interface;
 ISWbemLocatorDisp = dispinterface;
 ISWbemLastError = interface;
 ISWbemLastErrorDisp = dispinterface;
 ISWbemSinkEvents = dispinterface;
 ISWbemSink = interface;
 ISWbemSinkDisp = dispinterface;
 ISWbemServicesEx = interface;
 ISWbemServicesExDisp = dispinterface;
 ISWbemObjectEx = interface;
 ISWbemObjectExDisp = dispinterface;
 ISWbemDateTime = interface;
 ISWbemDateTimeDisp = dispinterface;
 ISWbemRefresher = interface;
 ISWbemRefresherDisp = dispinterface;
 ISWbemRefreshableItem = interface;
 ISWbemRefreshableItemDisp = dispinterface;

//Map CoClass to its default interface

 SWbemLocator = ISWbemLocator;
 SWbemNamedValueSet = ISWbemNamedValueSet;
 SWbemObjectPath = ISWbemObjectPath;
 SWbemLastError = ISWbemLastError;
 SWbemSink = ISWbemSink;
 SWbemDateTime = ISWbemDateTime;
 SWbemRefresher = ISWbemRefresher;
 SWbemServices = ISWbemServices;
 SWbemServicesEx = ISWbemServicesEx;
 SWbemObject = ISWbemObject;
 SWbemObjectEx = ISWbemObjectEx;
 SWbemObjectSet = ISWbemObjectSet;
 SWbemNamedValue = ISWbemNamedValue;
 SWbemQualifier = ISWbemQualifier;
 SWbemQualifierSet = ISWbemQualifierSet;
 SWbemProperty = ISWbemProperty;
 SWbemPropertySet = ISWbemPropertySet;
 SWbemMethod = ISWbemMethod;
 SWbemMethodSet = ISWbemMethodSet;
 SWbemEventSource = ISWbemEventSource;
 SWbemSecurity = ISWbemSecurity;
 SWbemPrivilege = ISWbemPrivilege;
 SWbemPrivilegeSet = ISWbemPrivilegeSet;
 SWbemRefreshableItem = ISWbemRefreshableItem;

//records, unions, aliases


//interface declarations

// ISWbemServices : A connection to a Namespace

 ISWbemServices = interface(IDispatch)
   ['{76A6415C-CB41-11D1-8B02-00600806D9B6}']
    // Get : Get a single Class or Instance 
   function Get(strObjectPath:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObject;safecall;
    // GetAsync : Get a single Class or Instance asynchronously 
   procedure GetAsync(objWbemSink:IDispatch;strObjectPath:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);safecall;
    // Delete : Delete a Class or Instance 
   procedure Delete(strObjectPath:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch);safecall;
    // DeleteAsync : Delete a Class or Instance asynchronously 
   procedure DeleteAsync(objWbemSink:IDispatch;strObjectPath:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);safecall;
    // InstancesOf : Enumerate the Instances of a Class 
   function InstancesOf(strClass:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;safecall;
    // InstancesOfAsync : Enumerate the Instances of a Class asynchronously 
   procedure InstancesOfAsync(objWbemSink:IDispatch;strClass:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);safecall;
    // SubclassesOf : Enumerate the subclasses of a Class 
   function SubclassesOf(strSuperclass:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;safecall;
    // SubclassesOfAsync : Enumerate the subclasses of a Class asynchronously  
   procedure SubclassesOfAsync(objWbemSink:IDispatch;strSuperclass:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);safecall;
    // ExecQuery : Execute a Query 
   function ExecQuery(strQuery:WideString;strQueryLanguage:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;safecall;
    // ExecQueryAsync : Execute an asynchronous Query 
   procedure ExecQueryAsync(objWbemSink:IDispatch;strQuery:WideString;strQueryLanguage:WideString;lFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);safecall;
    // AssociatorsOf : Get the Associators of a class or instance 
   function AssociatorsOf(strObjectPath:WideString;strAssocClass:WideString;strResultClass:WideString;strResultRole:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredAssocQualifier:WideString;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;safecall;
    // AssociatorsOfAsync : Get the Associators of a class or instance asynchronously 
   procedure AssociatorsOfAsync(objWbemSink:IDispatch;strObjectPath:WideString;strAssocClass:WideString;strResultClass:WideString;strResultRole:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredAssocQualifier:WideString;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);safecall;
    // ReferencesTo : Get the References to a class or instance 
   function ReferencesTo(strObjectPath:WideString;strResultClass:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;safecall;
    // ReferencesToAsync : Get the References to a class or instance asynchronously 
   procedure ReferencesToAsync(objWbemSink:IDispatch;strObjectPath:WideString;strResultClass:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);safecall;
    // ExecNotificationQuery : Execute a Query to receive Notifications 
   function ExecNotificationQuery(strQuery:WideString;strQueryLanguage:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemEventSource;safecall;
    // ExecNotificationQueryAsync : Execute an asynchronous Query to receive Notifications 
   procedure ExecNotificationQueryAsync(objWbemSink:IDispatch;strQuery:WideString;strQueryLanguage:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);safecall;
    // ExecMethod : Execute a Method 
   function ExecMethod(strObjectPath:WideString;strMethodName:WideString;objWbemInParameters:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObject;safecall;
    // ExecMethodAsync : Execute a Method asynchronously 
   procedure ExecMethodAsync(objWbemSink:IDispatch;strObjectPath:WideString;strMethodName:WideString;objWbemInParameters:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);safecall;
   function Get_Security_ : ISWbemSecurity; safecall;
    // Security_ : The Security Configurator for this Object 
   property Security_:ISWbemSecurity read Get_Security_;
  end;


// ISWbemServices : A connection to a Namespace

 ISWbemServicesDisp = dispinterface
   ['{76A6415C-CB41-11D1-8B02-00600806D9B6}']
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
    // Get : Get a single Class or Instance 
   function Get(strObjectPath:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObject;dispid 1;
    // GetAsync : Get a single Class or Instance asynchronously 
   procedure GetAsync(objWbemSink:IDispatch;strObjectPath:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 2;
    // Delete : Delete a Class or Instance 
   procedure Delete(strObjectPath:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch);dispid 3;
    // DeleteAsync : Delete a Class or Instance asynchronously 
   procedure DeleteAsync(objWbemSink:IDispatch;strObjectPath:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 4;
    // InstancesOf : Enumerate the Instances of a Class 
   function InstancesOf(strClass:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;dispid 5;
    // InstancesOfAsync : Enumerate the Instances of a Class asynchronously 
   procedure InstancesOfAsync(objWbemSink:IDispatch;strClass:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 6;
    // SubclassesOf : Enumerate the subclasses of a Class 
   function SubclassesOf(strSuperclass:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;dispid 7;
    // SubclassesOfAsync : Enumerate the subclasses of a Class asynchronously  
   procedure SubclassesOfAsync(objWbemSink:IDispatch;strSuperclass:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 8;
    // ExecQuery : Execute a Query 
   function ExecQuery(strQuery:WideString;strQueryLanguage:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;dispid 9;
    // ExecQueryAsync : Execute an asynchronous Query 
   procedure ExecQueryAsync(objWbemSink:IDispatch;strQuery:WideString;strQueryLanguage:WideString;lFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 10;
    // AssociatorsOf : Get the Associators of a class or instance 
   function AssociatorsOf(strObjectPath:WideString;strAssocClass:WideString;strResultClass:WideString;strResultRole:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredAssocQualifier:WideString;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;dispid 11;
    // AssociatorsOfAsync : Get the Associators of a class or instance asynchronously 
   procedure AssociatorsOfAsync(objWbemSink:IDispatch;strObjectPath:WideString;strAssocClass:WideString;strResultClass:WideString;strResultRole:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredAssocQualifier:WideString;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 12;
    // ReferencesTo : Get the References to a class or instance 
   function ReferencesTo(strObjectPath:WideString;strResultClass:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;dispid 13;
    // ReferencesToAsync : Get the References to a class or instance asynchronously 
   procedure ReferencesToAsync(objWbemSink:IDispatch;strObjectPath:WideString;strResultClass:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 14;
    // ExecNotificationQuery : Execute a Query to receive Notifications 
   function ExecNotificationQuery(strQuery:WideString;strQueryLanguage:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemEventSource;dispid 15;
    // ExecNotificationQueryAsync : Execute an asynchronous Query to receive Notifications 
   procedure ExecNotificationQueryAsync(objWbemSink:IDispatch;strQuery:WideString;strQueryLanguage:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 16;
    // ExecMethod : Execute a Method 
   function ExecMethod(strObjectPath:WideString;strMethodName:WideString;objWbemInParameters:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObject;dispid 17;
    // ExecMethodAsync : Execute a Method asynchronously 
   procedure ExecMethodAsync(objWbemSink:IDispatch;strObjectPath:WideString;strMethodName:WideString;objWbemInParameters:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 18;
    // Security_ : The Security Configurator for this Object 
   property Security_:ISWbemSecurity  readonly dispid 19;
  end;


// ISWbemObject : A Class or Instance

 ISWbemObject = interface(IDispatch)
   ['{76A6415A-CB41-11D1-8B02-00600806D9B6}']
    // Put_ : Save this Object 
   function Put_(iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectPath;safecall;
    // PutAsync_ : Save this Object asynchronously 
   procedure PutAsync_(objWbemSink:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);safecall;
    // Delete_ : Delete this Object 
   procedure Delete_(iFlags:Integer;objWbemNamedValueSet:IDispatch);safecall;
    // DeleteAsync_ : Delete this Object asynchronously 
   procedure DeleteAsync_(objWbemSink:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);safecall;
    // Instances_ : Return all instances of this Class 
   function Instances_(iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;safecall;
    // InstancesAsync_ : Return all instances of this Class asynchronously 
   procedure InstancesAsync_(objWbemSink:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);safecall;
    // Subclasses_ : Enumerate subclasses of this Class 
   function Subclasses_(iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;safecall;
    // SubclassesAsync_ : Enumerate subclasses of this Class asynchronously 
   procedure SubclassesAsync_(objWbemSink:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);safecall;
    // Associators_ : Get the Associators of this Object 
   function Associators_(strAssocClass:WideString;strResultClass:WideString;strResultRole:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredAssocQualifier:WideString;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;safecall;
    // AssociatorsAsync_ : Get the Associators of this Object asynchronously 
   procedure AssociatorsAsync_(objWbemSink:IDispatch;strAssocClass:WideString;strResultClass:WideString;strResultRole:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredAssocQualifier:WideString;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);safecall;
    // References_ : Get the References to this Object 
   function References_(strResultClass:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;safecall;
    // ReferencesAsync_ : Get the References to this Object asynchronously 
   procedure ReferencesAsync_(objWbemSink:IDispatch;strResultClass:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);safecall;
    // ExecMethod_ : Execute a Method of this Object 
   function ExecMethod_(strMethodName:WideString;objWbemInParameters:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObject;safecall;
    // ExecMethodAsync_ : Execute a Method of this Object asynchronously 
   procedure ExecMethodAsync_(objWbemSink:IDispatch;strMethodName:WideString;objWbemInParameters:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);safecall;
    // Clone_ : Clone this Object 
   function Clone_:ISWbemObject;safecall;
    // GetObjectText_ : Get the MOF text of this Object 
   function GetObjectText_(iFlags:Integer):WideString;safecall;
    // SpawnDerivedClass_ : Create a subclass of this Object 
   function SpawnDerivedClass_(iFlags:Integer):ISWbemObject;safecall;
    // SpawnInstance_ : Create an Instance of this Object 
   function SpawnInstance_(iFlags:Integer):ISWbemObject;safecall;
    // CompareTo_ : Compare this Object with another 
   function CompareTo_(objWbemObject:IDispatch;iFlags:Integer):WordBool;safecall;
   function Get_Qualifiers_ : ISWbemQualifierSet; safecall;
   function Get_Properties_ : ISWbemPropertySet; safecall;
   function Get_Methods_ : ISWbemMethodSet; safecall;
   function Get_Derivation_ : OleVariant; safecall;
   function Get_Path_ : ISWbemObjectPath; safecall;
   function Get_Security_ : ISWbemSecurity; safecall;
    // Qualifiers_ : The collection of Qualifiers of this Object 
   property Qualifiers_:ISWbemQualifierSet read Get_Qualifiers_;
    // Properties_ : The collection of Properties of this Object 
   property Properties_:ISWbemPropertySet read Get_Properties_;
    // Methods_ : The collection of Methods of this Object 
   property Methods_:ISWbemMethodSet read Get_Methods_;
    // Derivation_ : An array of strings describing the class derivation heirarchy, in most-derived-from order (the first element in the array defines the superclass and the last element defines the dynasty class). 
   property Derivation_:OleVariant read Get_Derivation_;
    // Path_ : The path of this Object 
   property Path_:ISWbemObjectPath read Get_Path_;
    // Security_ : The Security Configurator for this Object 
   property Security_:ISWbemSecurity read Get_Security_;
  end;


// ISWbemObject : A Class or Instance

 ISWbemObjectDisp = dispinterface
   ['{76A6415A-CB41-11D1-8B02-00600806D9B6}']
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
    // Put_ : Save this Object 
   function Put_(iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectPath;dispid 1;
    // PutAsync_ : Save this Object asynchronously 
   procedure PutAsync_(objWbemSink:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 2;
    // Delete_ : Delete this Object 
   procedure Delete_(iFlags:Integer;objWbemNamedValueSet:IDispatch);dispid 3;
    // DeleteAsync_ : Delete this Object asynchronously 
   procedure DeleteAsync_(objWbemSink:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 4;
    // Instances_ : Return all instances of this Class 
   function Instances_(iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;dispid 5;
    // InstancesAsync_ : Return all instances of this Class asynchronously 
   procedure InstancesAsync_(objWbemSink:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 6;
    // Subclasses_ : Enumerate subclasses of this Class 
   function Subclasses_(iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;dispid 7;
    // SubclassesAsync_ : Enumerate subclasses of this Class asynchronously 
   procedure SubclassesAsync_(objWbemSink:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 8;
    // Associators_ : Get the Associators of this Object 
   function Associators_(strAssocClass:WideString;strResultClass:WideString;strResultRole:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredAssocQualifier:WideString;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;dispid 9;
    // AssociatorsAsync_ : Get the Associators of this Object asynchronously 
   procedure AssociatorsAsync_(objWbemSink:IDispatch;strAssocClass:WideString;strResultClass:WideString;strResultRole:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredAssocQualifier:WideString;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 10;
    // References_ : Get the References to this Object 
   function References_(strResultClass:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;dispid 11;
    // ReferencesAsync_ : Get the References to this Object asynchronously 
   procedure ReferencesAsync_(objWbemSink:IDispatch;strResultClass:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 12;
    // ExecMethod_ : Execute a Method of this Object 
   function ExecMethod_(strMethodName:WideString;objWbemInParameters:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObject;dispid 13;
    // ExecMethodAsync_ : Execute a Method of this Object asynchronously 
   procedure ExecMethodAsync_(objWbemSink:IDispatch;strMethodName:WideString;objWbemInParameters:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 14;
    // Clone_ : Clone this Object 
   function Clone_:ISWbemObject;dispid 15;
    // GetObjectText_ : Get the MOF text of this Object 
   function GetObjectText_(iFlags:Integer):WideString;dispid 16;
    // SpawnDerivedClass_ : Create a subclass of this Object 
   function SpawnDerivedClass_(iFlags:Integer):ISWbemObject;dispid 17;
    // SpawnInstance_ : Create an Instance of this Object 
   function SpawnInstance_(iFlags:Integer):ISWbemObject;dispid 18;
    // CompareTo_ : Compare this Object with another 
   function CompareTo_(objWbemObject:IDispatch;iFlags:Integer):WordBool;dispid 19;
    // Qualifiers_ : The collection of Qualifiers of this Object 
   property Qualifiers_:ISWbemQualifierSet  readonly dispid 20;
    // Properties_ : The collection of Properties of this Object 
   property Properties_:ISWbemPropertySet  readonly dispid 21;
    // Methods_ : The collection of Methods of this Object 
   property Methods_:ISWbemMethodSet  readonly dispid 22;
    // Derivation_ : An array of strings describing the class derivation heirarchy, in most-derived-from order (the first element in the array defines the superclass and the last element defines the dynasty class). 
   property Derivation_:OleVariant  readonly dispid 23;
    // Path_ : The path of this Object 
   property Path_:ISWbemObjectPath  readonly dispid 24;
    // Security_ : The Security Configurator for this Object 
   property Security_:ISWbemSecurity  readonly dispid 25;
  end;


// ISWbemObjectPath : An Object path

 ISWbemObjectPath = interface(IDispatch)
   ['{5791BC27-CE9C-11D1-97BF-0000F81E849C}']
   function Get_Path : WideString; safecall;
   procedure Set_Path(const strPath:WideString); safecall;
   function Get_RelPath : WideString; safecall;
   procedure Set_RelPath(const strRelPath:WideString); safecall;
   function Get_Server : WideString; safecall;
   procedure Set_Server(const strServer:WideString); safecall;
   function Get_Namespace : WideString; safecall;
   procedure Set_Namespace(const strNamespace:WideString); safecall;
   function Get_ParentNamespace : WideString; safecall;
   function Get_DisplayName : WideString; safecall;
   procedure Set_DisplayName(const strDisplayName:WideString); safecall;
   function Get_Class_ : WideString; safecall;
   procedure Set_Class_(const strClass:WideString); safecall;
   function Get_IsClass : WordBool; safecall;
    // SetAsClass : Coerce this path to address a Class 
   procedure SetAsClass;safecall;
   function Get_IsSingleton : WordBool; safecall;
    // SetAsSingleton : Coerce this path to address a Singleton Instance 
   procedure SetAsSingleton;safecall;
   function Get_Keys : ISWbemNamedValueSet; safecall;
   function Get_Security_ : ISWbemSecurity; safecall;
   function Get_Locale : WideString; safecall;
   procedure Set_Locale(const strLocale:WideString); safecall;
   function Get_Authority : WideString; safecall;
   procedure Set_Authority(const strAuthority:WideString); safecall;
    // Path : The full path 
   property Path:WideString read Get_Path write Set_Path;
    // RelPath : The relative path 
   property RelPath:WideString read Get_RelPath write Set_RelPath;
    // Server : The name of the Server 
   property Server:WideString read Get_Server write Set_Server;
    // Namespace : The Namespace path 
   property Namespace:WideString read Get_Namespace write Set_Namespace;
    // ParentNamespace : The parent Namespace path 
   property ParentNamespace:WideString read Get_ParentNamespace;
    // DisplayName : The Display Name for this path 
   property DisplayName:WideString read Get_DisplayName write Set_DisplayName;
    // Class : The Class name 
   property Class_:WideString read Get_Class_ write Set_Class_;
    // IsClass : Indicates whether this path addresses a Class 
   property IsClass:WordBool read Get_IsClass;
    // IsSingleton : Indicates whether this path addresses a Singleton Instance 
   property IsSingleton:WordBool read Get_IsSingleton;
    // Keys : The collection of Key value bindings for this path 
   property Keys:ISWbemNamedValueSet read Get_Keys;
    // Security_ : Defines the security components of this path 
   property Security_:ISWbemSecurity read Get_Security_;
    // Locale : Defines locale component of this path 
   property Locale:WideString read Get_Locale write Set_Locale;
    // Authority : Defines authentication authority component of this path 
   property Authority:WideString read Get_Authority write Set_Authority;
  end;


// ISWbemObjectPath : An Object path

 ISWbemObjectPathDisp = dispinterface
   ['{5791BC27-CE9C-11D1-97BF-0000F81E849C}']
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
    // SetAsClass : Coerce this path to address a Class 
   procedure SetAsClass;dispid 8;
    // SetAsSingleton : Coerce this path to address a Singleton Instance 
   procedure SetAsSingleton;dispid 10;
    // Path : The full path 
   property Path:WideString dispid 0;
    // RelPath : The relative path 
   property RelPath:WideString dispid 1;
    // Server : The name of the Server 
   property Server:WideString dispid 2;
    // Namespace : The Namespace path 
   property Namespace:WideString dispid 3;
    // ParentNamespace : The parent Namespace path 
   property ParentNamespace:WideString  readonly dispid 4;
    // DisplayName : The Display Name for this path 
   property DisplayName:WideString dispid 5;
    // Class : The Class name 
   property Class_:WideString dispid 6;
    // IsClass : Indicates whether this path addresses a Class 
   property IsClass:WordBool  readonly dispid 7;
    // IsSingleton : Indicates whether this path addresses a Singleton Instance 
   property IsSingleton:WordBool  readonly dispid 9;
    // Keys : The collection of Key value bindings for this path 
   property Keys:ISWbemNamedValueSet  readonly dispid 11;
    // Security_ : Defines the security components of this path 
   property Security_:ISWbemSecurity  readonly dispid 12;
    // Locale : Defines locale component of this path 
   property Locale:WideString dispid 13;
    // Authority : Defines authentication authority component of this path 
   property Authority:WideString dispid 14;
  end;


// ISWbemNamedValueSet : A collection of named values

 ISWbemNamedValueSet = interface(IDispatch)
   ['{CF2376EA-CE8C-11D1-8B05-00600806D9B6}']
   function Get__NewEnum : IUnknown; safecall;
    // Item : Get a named value from this Collection 
   function Item(strName:WideString;iFlags:Integer):ISWbemNamedValue;safecall;
   function Get_Count : Integer; safecall;
    // Add : Add a named value to this collection 
   function Add(strName:WideString;var varValue:OleVariant;iFlags:Integer):ISWbemNamedValue;safecall;
    // Remove : Remove a named value from this collection 
   procedure Remove(strName:WideString;iFlags:Integer);safecall;
    // Clone : Make a copy of this collection 
   function Clone:ISWbemNamedValueSet;safecall;
    // DeleteAll : Delete all items in this collection 
   procedure DeleteAll;safecall;
    // _NewEnum :  
   property _NewEnum:IUnknown read Get__NewEnum;
    // Count : The number of items in this collection 
   property Count:Integer read Get_Count;
  end;


// ISWbemNamedValueSet : A collection of named values

 ISWbemNamedValueSetDisp = dispinterface
   ['{CF2376EA-CE8C-11D1-8B05-00600806D9B6}']
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
    // Item : Get a named value from this Collection 
   function Item(strName:WideString;iFlags:Integer):ISWbemNamedValue;dispid 0;
    // Add : Add a named value to this collection 
   function Add(strName:WideString;var varValue:OleVariant;iFlags:Integer):ISWbemNamedValue;dispid 2;
    // Remove : Remove a named value from this collection 
   procedure Remove(strName:WideString;iFlags:Integer);dispid 3;
    // Clone : Make a copy of this collection 
   function Clone:ISWbemNamedValueSet;dispid 4;
    // DeleteAll : Delete all items in this collection 
   procedure DeleteAll;dispid 5;
    // _NewEnum :  
   property _NewEnum:IUnknown  readonly dispid -4;
    // Count : The number of items in this collection 
   property Count:Integer  readonly dispid 1;
  end;


// ISWbemNamedValue : A named value

 ISWbemNamedValue = interface(IDispatch)
   ['{76A64164-CB41-11D1-8B02-00600806D9B6}']
   function Get_Value : OleVariant; safecall;
   procedure Set_Value(var varValue:POleVariant); safecall;
   function Get_Name : WideString; safecall;
    // Value : The Value of this Named element 
   property Value:OleVariant read Get_Value;
    // Name : The Name of this Value 
   property Name:WideString read Get_Name;
  end;


// ISWbemNamedValue : A named value

 ISWbemNamedValueDisp = dispinterface
   ['{76A64164-CB41-11D1-8B02-00600806D9B6}']
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
    // Value : The Value of this Named element 
   property Value:OleVariant dispid 0;
    // Name : The Name of this Value 
   property Name:WideString  readonly dispid 2;
  end;


// ISWbemSecurity : A Security Configurator

 ISWbemSecurity = interface(IDispatch)
   ['{B54D66E6-2287-11D2-8B33-00600806D9B6}']
   function Get_ImpersonationLevel : WbemImpersonationLevelEnum; safecall;
   procedure Set_ImpersonationLevel(const iImpersonationLevel:WbemImpersonationLevelEnum); safecall;
   function Get_AuthenticationLevel : WbemAuthenticationLevelEnum; safecall;
   procedure Set_AuthenticationLevel(const iAuthenticationLevel:WbemAuthenticationLevelEnum); safecall;
   function Get_Privileges : ISWbemPrivilegeSet; safecall;
    // ImpersonationLevel : The security impersonation level 
   property ImpersonationLevel:WbemImpersonationLevelEnum read Get_ImpersonationLevel write Set_ImpersonationLevel;
    // AuthenticationLevel : The security authentication level 
   property AuthenticationLevel:WbemAuthenticationLevelEnum read Get_AuthenticationLevel write Set_AuthenticationLevel;
    // Privileges : The collection of privileges for this object 
   property Privileges:ISWbemPrivilegeSet read Get_Privileges;
  end;


// ISWbemSecurity : A Security Configurator

 ISWbemSecurityDisp = dispinterface
   ['{B54D66E6-2287-11D2-8B33-00600806D9B6}']
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
    // ImpersonationLevel : The security impersonation level 
   property ImpersonationLevel:WbemImpersonationLevelEnum dispid 1;
    // AuthenticationLevel : The security authentication level 
   property AuthenticationLevel:WbemAuthenticationLevelEnum dispid 2;
    // Privileges : The collection of privileges for this object 
   property Privileges:ISWbemPrivilegeSet  readonly dispid 3;
  end;


// ISWbemPrivilegeSet : A collection of Privilege Overrides

 ISWbemPrivilegeSet = interface(IDispatch)
   ['{26EE67BF-5804-11D2-8B4A-00600806D9B6}']
   function Get__NewEnum : IUnknown; safecall;
    // Item : Get a named Privilege from this collection 
   function Item(iPrivilege:WbemPrivilegeEnum):ISWbemPrivilege;safecall;
   function Get_Count : Integer; safecall;
    // Add : Add a Privilege to this collection 
   function Add(iPrivilege:WbemPrivilegeEnum;bIsEnabled:WordBool):ISWbemPrivilege;safecall;
    // Remove : Remove a Privilege from this collection 
   procedure Remove(iPrivilege:WbemPrivilegeEnum);safecall;
    // DeleteAll : Delete all items in this collection 
   procedure DeleteAll;safecall;
    // AddAsString : Add a named Privilege to this collection 
   function AddAsString(strPrivilege:WideString;bIsEnabled:WordBool):ISWbemPrivilege;safecall;
    // _NewEnum :  
   property _NewEnum:IUnknown read Get__NewEnum;
    // Count : The number of items in this collection 
   property Count:Integer read Get_Count;
  end;


// ISWbemPrivilegeSet : A collection of Privilege Overrides

 ISWbemPrivilegeSetDisp = dispinterface
   ['{26EE67BF-5804-11D2-8B4A-00600806D9B6}']
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
    // Item : Get a named Privilege from this collection 
   function Item(iPrivilege:WbemPrivilegeEnum):ISWbemPrivilege;dispid 0;
    // Add : Add a Privilege to this collection 
   function Add(iPrivilege:WbemPrivilegeEnum;bIsEnabled:WordBool):ISWbemPrivilege;dispid 2;
    // Remove : Remove a Privilege from this collection 
   procedure Remove(iPrivilege:WbemPrivilegeEnum);dispid 3;
    // DeleteAll : Delete all items in this collection 
   procedure DeleteAll;dispid 4;
    // AddAsString : Add a named Privilege to this collection 
   function AddAsString(strPrivilege:WideString;bIsEnabled:WordBool):ISWbemPrivilege;dispid 5;
    // _NewEnum :  
   property _NewEnum:IUnknown  readonly dispid -4;
    // Count : The number of items in this collection 
   property Count:Integer  readonly dispid 1;
  end;


// ISWbemPrivilege : A Privilege Override

 ISWbemPrivilege = interface(IDispatch)
   ['{26EE67BD-5804-11D2-8B4A-00600806D9B6}']
   function Get_IsEnabled : WordBool; safecall;
   procedure Set_IsEnabled(const bIsEnabled:WordBool); safecall;
   function Get_Name : WideString; safecall;
   function Get_DisplayName : WideString; safecall;
   function Get_Identifier : WbemPrivilegeEnum; safecall;
    // IsEnabled : Whether the Privilege is to be enabled or disabled 
   property IsEnabled:WordBool read Get_IsEnabled write Set_IsEnabled;
    // Name : The name of the Privilege 
   property Name:WideString read Get_Name;
    // DisplayName : The display name of the Privilege 
   property DisplayName:WideString read Get_DisplayName;
    // Identifier : The Privilege identifier 
   property Identifier:WbemPrivilegeEnum read Get_Identifier;
  end;


// ISWbemPrivilege : A Privilege Override

 ISWbemPrivilegeDisp = dispinterface
   ['{26EE67BD-5804-11D2-8B4A-00600806D9B6}']
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
    // IsEnabled : Whether the Privilege is to be enabled or disabled 
   property IsEnabled:WordBool dispid 0;
    // Name : The name of the Privilege 
   property Name:WideString  readonly dispid 1;
    // DisplayName : The display name of the Privilege 
   property DisplayName:WideString  readonly dispid 2;
    // Identifier : The Privilege identifier 
   property Identifier:WbemPrivilegeEnum  readonly dispid 3;
  end;


// ISWbemObjectSet : A collection of Classes or Instances

 ISWbemObjectSet = interface(IDispatch)
   ['{76A6415F-CB41-11D1-8B02-00600806D9B6}']
   function Get__NewEnum : IUnknown; safecall;
    // Item : Get an Object with a specific path from this collection 
   function Item(strObjectPath:WideString;iFlags:Integer):ISWbemObject;safecall;
   function Get_Count : Integer; safecall;
   function Get_Security_ : ISWbemSecurity; safecall;
    // ItemIndex : Get an Object with a specific index from this collection 
   function ItemIndex(lIndex:Integer):ISWbemObject;safecall;
    // _NewEnum :  
   property _NewEnum:IUnknown read Get__NewEnum;
    // Count : The number of items in this collection 
   property Count:Integer read Get_Count;
    // Security_ : The Security Configurator for this Object 
   property Security_:ISWbemSecurity read Get_Security_;
  end;


// ISWbemObjectSet : A collection of Classes or Instances

 ISWbemObjectSetDisp = dispinterface
   ['{76A6415F-CB41-11D1-8B02-00600806D9B6}']
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
    // Item : Get an Object with a specific path from this collection 
   function Item(strObjectPath:WideString;iFlags:Integer):ISWbemObject;dispid 0;
    // ItemIndex : Get an Object with a specific index from this collection 
   function ItemIndex(lIndex:Integer):ISWbemObject;dispid 5;
    // _NewEnum :  
   property _NewEnum:IUnknown  readonly dispid -4;
    // Count : The number of items in this collection 
   property Count:Integer  readonly dispid 1;
    // Security_ : The Security Configurator for this Object 
   property Security_:ISWbemSecurity  readonly dispid 4;
  end;


// ISWbemQualifierSet : A collection of Qualifiers

 ISWbemQualifierSet = interface(IDispatch)
   ['{9B16ED16-D3DF-11D1-8B08-00600806D9B6}']
   function Get__NewEnum : IUnknown; safecall;
    // Item : Get a named Qualifier from this collection 
   function Item(Name:WideString;iFlags:Integer):ISWbemQualifier;safecall;
   function Get_Count : Integer; safecall;
    // Add : Add a Qualifier to this collection 
   function Add(strName:WideString;var varVal:OleVariant;bPropagatesToSubclass:WordBool;bPropagatesToInstance:WordBool;bIsOverridable:WordBool;iFlags:Integer):ISWbemQualifier;safecall;
    // Remove : Remove a Qualifier from this collection 
   procedure Remove(strName:WideString;iFlags:Integer);safecall;
    // _NewEnum :  
   property _NewEnum:IUnknown read Get__NewEnum;
    // Count : The number of items in this collection 
   property Count:Integer read Get_Count;
  end;


// ISWbemQualifierSet : A collection of Qualifiers

 ISWbemQualifierSetDisp = dispinterface
   ['{9B16ED16-D3DF-11D1-8B08-00600806D9B6}']
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
    // Item : Get a named Qualifier from this collection 
   function Item(Name:WideString;iFlags:Integer):ISWbemQualifier;dispid 0;
    // Add : Add a Qualifier to this collection 
   function Add(strName:WideString;var varVal:OleVariant;bPropagatesToSubclass:WordBool;bPropagatesToInstance:WordBool;bIsOverridable:WordBool;iFlags:Integer):ISWbemQualifier;dispid 2;
    // Remove : Remove a Qualifier from this collection 
   procedure Remove(strName:WideString;iFlags:Integer);dispid 3;
    // _NewEnum :  
   property _NewEnum:IUnknown  readonly dispid -4;
    // Count : The number of items in this collection 
   property Count:Integer  readonly dispid 1;
  end;


// ISWbemQualifier : A Qualifier

 ISWbemQualifier = interface(IDispatch)
   ['{79B05932-D3B7-11D1-8B06-00600806D9B6}']
   function Get_Value : OleVariant; safecall;
   procedure Set_Value(var varValue:POleVariant); safecall;
   function Get_Name : WideString; safecall;
   function Get_IsLocal : WordBool; safecall;
   function Get_PropagatesToSubclass : WordBool; safecall;
   procedure Set_PropagatesToSubclass(const bPropagatesToSubclass:WordBool); safecall;
   function Get_PropagatesToInstance : WordBool; safecall;
   procedure Set_PropagatesToInstance(const bPropagatesToInstance:WordBool); safecall;
   function Get_IsOverridable : WordBool; safecall;
   procedure Set_IsOverridable(const bIsOverridable:WordBool); safecall;
   function Get_IsAmended : WordBool; safecall;
    // Value : The value of this Qualifier 
   property Value:OleVariant read Get_Value;
    // Name : The name of this Qualifier 
   property Name:WideString read Get_Name;
    // IsLocal : Indicates whether this Qualifier is local or propagated 
   property IsLocal:WordBool read Get_IsLocal;
    // PropagatesToSubclass : Determines whether this Qualifier can propagate to subclasses 
   property PropagatesToSubclass:WordBool read Get_PropagatesToSubclass write Set_PropagatesToSubclass;
    // PropagatesToInstance : Determines whether this Qualifier can propagate to instances 
   property PropagatesToInstance:WordBool read Get_PropagatesToInstance write Set_PropagatesToInstance;
    // IsOverridable : Determines whether this Qualifier can be overridden where propagated 
   property IsOverridable:WordBool read Get_IsOverridable write Set_IsOverridable;
    // IsAmended : Determines whether the value of this Qualifier has been amended 
   property IsAmended:WordBool read Get_IsAmended;
  end;


// ISWbemQualifier : A Qualifier

 ISWbemQualifierDisp = dispinterface
   ['{79B05932-D3B7-11D1-8B06-00600806D9B6}']
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
    // Value : The value of this Qualifier 
   property Value:OleVariant dispid 0;
    // Name : The name of this Qualifier 
   property Name:WideString  readonly dispid 1;
    // IsLocal : Indicates whether this Qualifier is local or propagated 
   property IsLocal:WordBool  readonly dispid 2;
    // PropagatesToSubclass : Determines whether this Qualifier can propagate to subclasses 
   property PropagatesToSubclass:WordBool dispid 3;
    // PropagatesToInstance : Determines whether this Qualifier can propagate to instances 
   property PropagatesToInstance:WordBool dispid 4;
    // IsOverridable : Determines whether this Qualifier can be overridden where propagated 
   property IsOverridable:WordBool dispid 5;
    // IsAmended : Determines whether the value of this Qualifier has been amended 
   property IsAmended:WordBool  readonly dispid 6;
  end;


// ISWbemPropertySet : A collection of Properties

 ISWbemPropertySet = interface(IDispatch)
   ['{DEA0A7B2-D4BA-11D1-8B09-00600806D9B6}']
   function Get__NewEnum : IUnknown; safecall;
    // Item : Get a named Property from this collection 
   function Item(strName:WideString;iFlags:Integer):ISWbemProperty;safecall;
   function Get_Count : Integer; safecall;
    // Add : Add a Property to this collection 
   function Add(strName:WideString;iCimType:WbemCimtypeEnum;bIsArray:WordBool;iFlags:Integer):ISWbemProperty;safecall;
    // Remove : Remove a Property from this collection 
   procedure Remove(strName:WideString;iFlags:Integer);safecall;
    // _NewEnum :  
   property _NewEnum:IUnknown read Get__NewEnum;
    // Count : The number of items in this collection 
   property Count:Integer read Get_Count;
  end;


// ISWbemPropertySet : A collection of Properties

 ISWbemPropertySetDisp = dispinterface
   ['{DEA0A7B2-D4BA-11D1-8B09-00600806D9B6}']
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
    // Item : Get a named Property from this collection 
   function Item(strName:WideString;iFlags:Integer):ISWbemProperty;dispid 0;
    // Add : Add a Property to this collection 
   function Add(strName:WideString;iCimType:WbemCimtypeEnum;bIsArray:WordBool;iFlags:Integer):ISWbemProperty;dispid 2;
    // Remove : Remove a Property from this collection 
   procedure Remove(strName:WideString;iFlags:Integer);dispid 3;
    // _NewEnum :  
   property _NewEnum:IUnknown  readonly dispid -4;
    // Count : The number of items in this collection 
   property Count:Integer  readonly dispid 1;
  end;


// ISWbemProperty : A Property

 ISWbemProperty = interface(IDispatch)
   ['{1A388F98-D4BA-11D1-8B09-00600806D9B6}']
   function Get_Value : OleVariant; safecall;
   procedure Set_Value(var varValue:POleVariant); safecall;
   function Get_Name : WideString; safecall;
   function Get_IsLocal : WordBool; safecall;
   function Get_Origin : WideString; safecall;
   function Get_CIMType : WbemCimtypeEnum; safecall;
   function Get_Qualifiers_ : ISWbemQualifierSet; safecall;
   function Get_IsArray : WordBool; safecall;
    // Value : The value of this Property 
   property Value:OleVariant read Get_Value;
    // Name : The name of this Property 
   property Name:WideString read Get_Name;
    // IsLocal : Indicates whether this Property is local or propagated 
   property IsLocal:WordBool read Get_IsLocal;
    // Origin : The originating class of this Property 
   property Origin:WideString read Get_Origin;
    // CIMType : The CIM Type of this Property 
   property CIMType:WbemCimtypeEnum read Get_CIMType;
    // Qualifiers_ : The collection of Qualifiers of this Property 
   property Qualifiers_:ISWbemQualifierSet read Get_Qualifiers_;
    // IsArray : Indicates whether this Property is an array type 
   property IsArray:WordBool read Get_IsArray;
  end;


// ISWbemProperty : A Property

 ISWbemPropertyDisp = dispinterface
   ['{1A388F98-D4BA-11D1-8B09-00600806D9B6}']
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
    // Value : The value of this Property 
   property Value:OleVariant dispid 0;
    // Name : The name of this Property 
   property Name:WideString  readonly dispid 1;
    // IsLocal : Indicates whether this Property is local or propagated 
   property IsLocal:WordBool  readonly dispid 2;
    // Origin : The originating class of this Property 
   property Origin:WideString  readonly dispid 3;
    // CIMType : The CIM Type of this Property 
   property CIMType:WbemCimtypeEnum  readonly dispid 4;
    // Qualifiers_ : The collection of Qualifiers of this Property 
   property Qualifiers_:ISWbemQualifierSet  readonly dispid 5;
    // IsArray : Indicates whether this Property is an array type 
   property IsArray:WordBool  readonly dispid 6;
  end;


// ISWbemMethodSet : A collection of Methods

 ISWbemMethodSet = interface(IDispatch)
   ['{C93BA292-D955-11D1-8B09-00600806D9B6}']
   function Get__NewEnum : IUnknown; safecall;
    // Item : Get a named Method from this collection 
   function Item(strName:WideString;iFlags:Integer):ISWbemMethod;safecall;
   function Get_Count : Integer; safecall;
    // _NewEnum :  
   property _NewEnum:IUnknown read Get__NewEnum;
    // Count : The number of items in this collection 
   property Count:Integer read Get_Count;
  end;


// ISWbemMethodSet : A collection of Methods

 ISWbemMethodSetDisp = dispinterface
   ['{C93BA292-D955-11D1-8B09-00600806D9B6}']
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
    // Item : Get a named Method from this collection 
   function Item(strName:WideString;iFlags:Integer):ISWbemMethod;dispid 0;
    // _NewEnum :  
   property _NewEnum:IUnknown  readonly dispid -4;
    // Count : The number of items in this collection 
   property Count:Integer  readonly dispid 1;
  end;


// ISWbemMethod : A Method

 ISWbemMethod = interface(IDispatch)
   ['{422E8E90-D955-11D1-8B09-00600806D9B6}']
   function Get_Name : WideString; safecall;
   function Get_Origin : WideString; safecall;
   function Get_InParameters : ISWbemObject; safecall;
   function Get_OutParameters : ISWbemObject; safecall;
   function Get_Qualifiers_ : ISWbemQualifierSet; safecall;
    // Name : The name of this Method 
   property Name:WideString read Get_Name;
    // Origin : The originating class of this Method 
   property Origin:WideString read Get_Origin;
    // InParameters : The in parameters for this Method. 
   property InParameters:ISWbemObject read Get_InParameters;
    // OutParameters : The out parameters for this Method. 
   property OutParameters:ISWbemObject read Get_OutParameters;
    // Qualifiers_ : The collection of Qualifiers of this Method. 
   property Qualifiers_:ISWbemQualifierSet read Get_Qualifiers_;
  end;


// ISWbemMethod : A Method

 ISWbemMethodDisp = dispinterface
   ['{422E8E90-D955-11D1-8B09-00600806D9B6}']
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
    // Name : The name of this Method 
   property Name:WideString  readonly dispid 1;
    // Origin : The originating class of this Method 
   property Origin:WideString  readonly dispid 2;
    // InParameters : The in parameters for this Method. 
   property InParameters:ISWbemObject  readonly dispid 3;
    // OutParameters : The out parameters for this Method. 
   property OutParameters:ISWbemObject  readonly dispid 4;
    // Qualifiers_ : The collection of Qualifiers of this Method. 
   property Qualifiers_:ISWbemQualifierSet  readonly dispid 5;
  end;


// ISWbemEventSource : An Event source

 ISWbemEventSource = interface(IDispatch)
   ['{27D54D92-0EBE-11D2-8B22-00600806D9B6}']
    // NextEvent : Retrieve the next event within a specified time period. The timeout is specified in milliseconds. 
   function NextEvent(iTimeoutMs:Integer):ISWbemObject;safecall;
   function Get_Security_ : ISWbemSecurity; safecall;
    // Security_ : The Security Configurator for this Object 
   property Security_:ISWbemSecurity read Get_Security_;
  end;


// ISWbemEventSource : An Event source

 ISWbemEventSourceDisp = dispinterface
   ['{27D54D92-0EBE-11D2-8B22-00600806D9B6}']
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
    // NextEvent : Retrieve the next event within a specified time period. The timeout is specified in milliseconds. 
   function NextEvent(iTimeoutMs:Integer):ISWbemObject;dispid 1;
    // Security_ : The Security Configurator for this Object 
   property Security_:ISWbemSecurity  readonly dispid 2;
  end;


// ISWbemLocator : Used to obtain Namespace connections

 ISWbemLocator = interface(IDispatch)
   ['{76A6415B-CB41-11D1-8B02-00600806D9B6}']
    // ConnectServer : Connect to a Namespace 
   function ConnectServer(strServer:WideString;strNamespace:WideString;strUser:WideString;strPassword:WideString;strLocale:WideString;strAuthority:WideString;iSecurityFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemServices;safecall;
   function Get_Security_ : ISWbemSecurity; safecall;
    // Security_ : The Security Configurator for this Object 
   property Security_:ISWbemSecurity read Get_Security_;
  end;


// ISWbemLocator : Used to obtain Namespace connections

 ISWbemLocatorDisp = dispinterface
   ['{76A6415B-CB41-11D1-8B02-00600806D9B6}']
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
    // ConnectServer : Connect to a Namespace 
   function ConnectServer(strServer:WideString;strNamespace:WideString;strUser:WideString;strPassword:WideString;strLocale:WideString;strAuthority:WideString;iSecurityFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemServices;dispid 1;
    // Security_ : The Security Configurator for this Object 
   property Security_:ISWbemSecurity  readonly dispid 2;
  end;


// ISWbemLastError : The last error on the current thread

 ISWbemLastError = interface(ISWbemObject)
   ['{D962DB84-D4BB-11D1-8B09-00600806D9B6}']
  end;


// ISWbemLastError : The last error on the current thread

 ISWbemLastErrorDisp = dispinterface
   ['{D962DB84-D4BB-11D1-8B09-00600806D9B6}']
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
    // Put_ : Save this Object 
   function Put_(iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectPath;dispid 1;
    // PutAsync_ : Save this Object asynchronously 
   procedure PutAsync_(objWbemSink:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 2;
    // Delete_ : Delete this Object 
   procedure Delete_(iFlags:Integer;objWbemNamedValueSet:IDispatch);dispid 3;
    // DeleteAsync_ : Delete this Object asynchronously 
   procedure DeleteAsync_(objWbemSink:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 4;
    // Instances_ : Return all instances of this Class 
   function Instances_(iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;dispid 5;
    // InstancesAsync_ : Return all instances of this Class asynchronously 
   procedure InstancesAsync_(objWbemSink:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 6;
    // Subclasses_ : Enumerate subclasses of this Class 
   function Subclasses_(iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;dispid 7;
    // SubclassesAsync_ : Enumerate subclasses of this Class asynchronously 
   procedure SubclassesAsync_(objWbemSink:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 8;
    // Associators_ : Get the Associators of this Object 
   function Associators_(strAssocClass:WideString;strResultClass:WideString;strResultRole:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredAssocQualifier:WideString;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;dispid 9;
    // AssociatorsAsync_ : Get the Associators of this Object asynchronously 
   procedure AssociatorsAsync_(objWbemSink:IDispatch;strAssocClass:WideString;strResultClass:WideString;strResultRole:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredAssocQualifier:WideString;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 10;
    // References_ : Get the References to this Object 
   function References_(strResultClass:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;dispid 11;
    // ReferencesAsync_ : Get the References to this Object asynchronously 
   procedure ReferencesAsync_(objWbemSink:IDispatch;strResultClass:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 12;
    // ExecMethod_ : Execute a Method of this Object 
   function ExecMethod_(strMethodName:WideString;objWbemInParameters:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObject;dispid 13;
    // ExecMethodAsync_ : Execute a Method of this Object asynchronously 
   procedure ExecMethodAsync_(objWbemSink:IDispatch;strMethodName:WideString;objWbemInParameters:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 14;
    // Clone_ : Clone this Object 
   function Clone_:ISWbemObject;dispid 15;
    // GetObjectText_ : Get the MOF text of this Object 
   function GetObjectText_(iFlags:Integer):WideString;dispid 16;
    // SpawnDerivedClass_ : Create a subclass of this Object 
   function SpawnDerivedClass_(iFlags:Integer):ISWbemObject;dispid 17;
    // SpawnInstance_ : Create an Instance of this Object 
   function SpawnInstance_(iFlags:Integer):ISWbemObject;dispid 18;
    // CompareTo_ : Compare this Object with another 
   function CompareTo_(objWbemObject:IDispatch;iFlags:Integer):WordBool;dispid 19;
    // Qualifiers_ : The collection of Qualifiers of this Object 
   property Qualifiers_:ISWbemQualifierSet  readonly dispid 20;
    // Properties_ : The collection of Properties of this Object 
   property Properties_:ISWbemPropertySet  readonly dispid 21;
    // Methods_ : The collection of Methods of this Object 
   property Methods_:ISWbemMethodSet  readonly dispid 22;
    // Derivation_ : An array of strings describing the class derivation heirarchy, in most-derived-from order (the first element in the array defines the superclass and the last element defines the dynasty class). 
   property Derivation_:OleVariant  readonly dispid 23;
    // Path_ : The path of this Object 
   property Path_:ISWbemObjectPath  readonly dispid 24;
    // Security_ : The Security Configurator for this Object 
   property Security_:ISWbemSecurity  readonly dispid 25;
  end;


// ISWbemSinkEvents : A sink for events arising from asynchronous operations

 ISWbemSinkEvents = dispinterface
   ['{75718CA0-F029-11D1-A1AC-00C04FB6C223}']
    // OnObjectReady : Event triggered when an Object is available 
   procedure OnObjectReady(objWbemObject:ISWbemObject;objWbemAsyncContext:ISWbemNamedValueSet);dispid 1;
    // OnCompleted : Event triggered when an asynchronous operation is completed 
   procedure OnCompleted(iHResult:WbemErrorEnum;objWbemErrorObject:ISWbemObject;objWbemAsyncContext:ISWbemNamedValueSet);dispid 2;
    // OnProgress : Event triggered to report the progress of an asynchronous operation 
   procedure OnProgress(iUpperBound:Integer;iCurrent:Integer;strMessage:WideString;objWbemAsyncContext:ISWbemNamedValueSet);dispid 3;
    // OnObjectPut : Event triggered when an object path is available following a Put operation 
   procedure OnObjectPut(objWbemObjectPath:ISWbemObjectPath;objWbemAsyncContext:ISWbemNamedValueSet);dispid 4;
  end;


// ISWbemSink : Asynchronous operation control

 ISWbemSink = interface(IDispatch)
   ['{75718C9F-F029-11D1-A1AC-00C04FB6C223}']
    // Cancel : Cancel an asynchronous operation 
   procedure Cancel;safecall;
  end;


// ISWbemSink : Asynchronous operation control

 ISWbemSinkDisp = dispinterface
   ['{75718C9F-F029-11D1-A1AC-00C04FB6C223}']
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
    // Cancel : Cancel an asynchronous operation 
   procedure Cancel;dispid 1;
  end;


// ISWbemServicesEx : A connection to a Namespace

 ISWbemServicesEx = interface(ISWbemServices)
   ['{D2F68443-85DC-427E-91D8-366554CC754C}']
    // Put : Save the Object to this Namespace 
   function Put(objWbemObject:ISWbemObjectEx;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectPath;safecall;
    // PutAsync : Save the Object to this Namespace asynchronously 
   procedure PutAsync(objWbemSink:ISWbemSink;objWbemObject:ISWbemObjectEx;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);safecall;
  end;


// ISWbemServicesEx : A connection to a Namespace

 ISWbemServicesExDisp = dispinterface
   ['{D2F68443-85DC-427E-91D8-366554CC754C}']
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
    // Get : Get a single Class or Instance 
   function Get(strObjectPath:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObject;dispid 1;
    // GetAsync : Get a single Class or Instance asynchronously 
   procedure GetAsync(objWbemSink:IDispatch;strObjectPath:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 2;
    // Delete : Delete a Class or Instance 
   procedure Delete(strObjectPath:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch);dispid 3;
    // DeleteAsync : Delete a Class or Instance asynchronously 
   procedure DeleteAsync(objWbemSink:IDispatch;strObjectPath:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 4;
    // InstancesOf : Enumerate the Instances of a Class 
   function InstancesOf(strClass:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;dispid 5;
    // InstancesOfAsync : Enumerate the Instances of a Class asynchronously 
   procedure InstancesOfAsync(objWbemSink:IDispatch;strClass:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 6;
    // SubclassesOf : Enumerate the subclasses of a Class 
   function SubclassesOf(strSuperclass:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;dispid 7;
    // SubclassesOfAsync : Enumerate the subclasses of a Class asynchronously  
   procedure SubclassesOfAsync(objWbemSink:IDispatch;strSuperclass:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 8;
    // ExecQuery : Execute a Query 
   function ExecQuery(strQuery:WideString;strQueryLanguage:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;dispid 9;
    // ExecQueryAsync : Execute an asynchronous Query 
   procedure ExecQueryAsync(objWbemSink:IDispatch;strQuery:WideString;strQueryLanguage:WideString;lFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 10;
    // AssociatorsOf : Get the Associators of a class or instance 
   function AssociatorsOf(strObjectPath:WideString;strAssocClass:WideString;strResultClass:WideString;strResultRole:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredAssocQualifier:WideString;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;dispid 11;
    // AssociatorsOfAsync : Get the Associators of a class or instance asynchronously 
   procedure AssociatorsOfAsync(objWbemSink:IDispatch;strObjectPath:WideString;strAssocClass:WideString;strResultClass:WideString;strResultRole:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredAssocQualifier:WideString;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 12;
    // ReferencesTo : Get the References to a class or instance 
   function ReferencesTo(strObjectPath:WideString;strResultClass:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;dispid 13;
    // ReferencesToAsync : Get the References to a class or instance asynchronously 
   procedure ReferencesToAsync(objWbemSink:IDispatch;strObjectPath:WideString;strResultClass:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 14;
    // ExecNotificationQuery : Execute a Query to receive Notifications 
   function ExecNotificationQuery(strQuery:WideString;strQueryLanguage:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemEventSource;dispid 15;
    // ExecNotificationQueryAsync : Execute an asynchronous Query to receive Notifications 
   procedure ExecNotificationQueryAsync(objWbemSink:IDispatch;strQuery:WideString;strQueryLanguage:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 16;
    // ExecMethod : Execute a Method 
   function ExecMethod(strObjectPath:WideString;strMethodName:WideString;objWbemInParameters:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObject;dispid 17;
    // ExecMethodAsync : Execute a Method asynchronously 
   procedure ExecMethodAsync(objWbemSink:IDispatch;strObjectPath:WideString;strMethodName:WideString;objWbemInParameters:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 18;
    // Put : Save the Object to this Namespace 
   function Put(objWbemObject:ISWbemObjectEx;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectPath;dispid 20;
    // PutAsync : Save the Object to this Namespace asynchronously 
   procedure PutAsync(objWbemSink:ISWbemSink;objWbemObject:ISWbemObjectEx;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 21;
    // Security_ : The Security Configurator for this Object 
   property Security_:ISWbemSecurity  readonly dispid 19;
  end;


// ISWbemObjectEx : A Class or Instance

 ISWbemObjectEx = interface(ISWbemObject)
   ['{269AD56A-8A67-4129-BC8C-0506DCFE9880}']
    // Refresh_ : Refresh this Object 
   procedure Refresh_(iFlags:Integer;objWbemNamedValueSet:IDispatch);safecall;
   function Get_SystemProperties_ : ISWbemPropertySet; safecall;
    // GetText_ : Retrieve a textual representation of this Object 
   function GetText_(iObjectTextFormat:WbemObjectTextFormatEnum;iFlags:Integer;objWbemNamedValueSet:IDispatch):WideString;safecall;
    // SetFromText_ : Set this Object using the supplied textual representation 
   procedure SetFromText_(bsText:WideString;iObjectTextFormat:WbemObjectTextFormatEnum;iFlags:Integer;objWbemNamedValueSet:IDispatch);safecall;
    // SystemProperties_ : The collection of System Properties of this Object 
   property SystemProperties_:ISWbemPropertySet read Get_SystemProperties_;
  end;


// ISWbemObjectEx : A Class or Instance

 ISWbemObjectExDisp = dispinterface
   ['{269AD56A-8A67-4129-BC8C-0506DCFE9880}']
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
    // Put_ : Save this Object 
   function Put_(iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectPath;dispid 1;
    // PutAsync_ : Save this Object asynchronously 
   procedure PutAsync_(objWbemSink:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 2;
    // Delete_ : Delete this Object 
   procedure Delete_(iFlags:Integer;objWbemNamedValueSet:IDispatch);dispid 3;
    // DeleteAsync_ : Delete this Object asynchronously 
   procedure DeleteAsync_(objWbemSink:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 4;
    // Instances_ : Return all instances of this Class 
   function Instances_(iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;dispid 5;
    // InstancesAsync_ : Return all instances of this Class asynchronously 
   procedure InstancesAsync_(objWbemSink:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 6;
    // Subclasses_ : Enumerate subclasses of this Class 
   function Subclasses_(iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;dispid 7;
    // SubclassesAsync_ : Enumerate subclasses of this Class asynchronously 
   procedure SubclassesAsync_(objWbemSink:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 8;
    // Associators_ : Get the Associators of this Object 
   function Associators_(strAssocClass:WideString;strResultClass:WideString;strResultRole:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredAssocQualifier:WideString;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;dispid 9;
    // AssociatorsAsync_ : Get the Associators of this Object asynchronously 
   procedure AssociatorsAsync_(objWbemSink:IDispatch;strAssocClass:WideString;strResultClass:WideString;strResultRole:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredAssocQualifier:WideString;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 10;
    // References_ : Get the References to this Object 
   function References_(strResultClass:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObjectSet;dispid 11;
    // ReferencesAsync_ : Get the References to this Object asynchronously 
   procedure ReferencesAsync_(objWbemSink:IDispatch;strResultClass:WideString;strRole:WideString;bClassesOnly:WordBool;bSchemaOnly:WordBool;strRequiredQualifier:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 12;
    // ExecMethod_ : Execute a Method of this Object 
   function ExecMethod_(strMethodName:WideString;objWbemInParameters:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemObject;dispid 13;
    // ExecMethodAsync_ : Execute a Method of this Object asynchronously 
   procedure ExecMethodAsync_(objWbemSink:IDispatch;strMethodName:WideString;objWbemInParameters:IDispatch;iFlags:Integer;objWbemNamedValueSet:IDispatch;objWbemAsyncContext:IDispatch);dispid 14;
    // Clone_ : Clone this Object 
   function Clone_:ISWbemObject;dispid 15;
    // GetObjectText_ : Get the MOF text of this Object 
   function GetObjectText_(iFlags:Integer):WideString;dispid 16;
    // SpawnDerivedClass_ : Create a subclass of this Object 
   function SpawnDerivedClass_(iFlags:Integer):ISWbemObject;dispid 17;
    // SpawnInstance_ : Create an Instance of this Object 
   function SpawnInstance_(iFlags:Integer):ISWbemObject;dispid 18;
    // CompareTo_ : Compare this Object with another 
   function CompareTo_(objWbemObject:IDispatch;iFlags:Integer):WordBool;dispid 19;
    // Refresh_ : Refresh this Object 
   procedure Refresh_(iFlags:Integer;objWbemNamedValueSet:IDispatch);dispid 26;
    // GetText_ : Retrieve a textual representation of this Object 
   function GetText_(iObjectTextFormat:WbemObjectTextFormatEnum;iFlags:Integer;objWbemNamedValueSet:IDispatch):WideString;dispid 28;
    // SetFromText_ : Set this Object using the supplied textual representation 
   procedure SetFromText_(bsText:WideString;iObjectTextFormat:WbemObjectTextFormatEnum;iFlags:Integer;objWbemNamedValueSet:IDispatch);dispid 29;
    // Qualifiers_ : The collection of Qualifiers of this Object 
   property Qualifiers_:ISWbemQualifierSet  readonly dispid 20;
    // Properties_ : The collection of Properties of this Object 
   property Properties_:ISWbemPropertySet  readonly dispid 21;
    // Methods_ : The collection of Methods of this Object 
   property Methods_:ISWbemMethodSet  readonly dispid 22;
    // Derivation_ : An array of strings describing the class derivation heirarchy, in most-derived-from order (the first element in the array defines the superclass and the last element defines the dynasty class). 
   property Derivation_:OleVariant  readonly dispid 23;
    // Path_ : The path of this Object 
   property Path_:ISWbemObjectPath  readonly dispid 24;
    // Security_ : The Security Configurator for this Object 
   property Security_:ISWbemSecurity  readonly dispid 25;
    // SystemProperties_ : The collection of System Properties of this Object 
   property SystemProperties_:ISWbemPropertySet  readonly dispid 27;
  end;


// ISWbemDateTime : A Datetime

 ISWbemDateTime = interface(IDispatch)
   ['{5E97458A-CF77-11D3-B38F-00105A1F473A}']
   function Get_Value : WideString; safecall;
   procedure Set_Value(const strValue:WideString); safecall;
   function Get_Year : Integer; safecall;
   procedure Set_Year(const iYear:Integer); safecall;
   function Get_YearSpecified : WordBool; safecall;
   procedure Set_YearSpecified(const bYearSpecified:WordBool); safecall;
   function Get_Month : Integer; safecall;
   procedure Set_Month(const iMonth:Integer); safecall;
   function Get_MonthSpecified : WordBool; safecall;
   procedure Set_MonthSpecified(const bMonthSpecified:WordBool); safecall;
   function Get_Day : Integer; safecall;
   procedure Set_Day(const iDay:Integer); safecall;
   function Get_DaySpecified : WordBool; safecall;
   procedure Set_DaySpecified(const bDaySpecified:WordBool); safecall;
   function Get_Hours : Integer; safecall;
   procedure Set_Hours(const iHours:Integer); safecall;
   function Get_HoursSpecified : WordBool; safecall;
   procedure Set_HoursSpecified(const bHoursSpecified:WordBool); safecall;
   function Get_Minutes : Integer; safecall;
   procedure Set_Minutes(const iMinutes:Integer); safecall;
   function Get_MinutesSpecified : WordBool; safecall;
   procedure Set_MinutesSpecified(const bMinutesSpecified:WordBool); safecall;
   function Get_Seconds : Integer; safecall;
   procedure Set_Seconds(const iSeconds:Integer); safecall;
   function Get_SecondsSpecified : WordBool; safecall;
   procedure Set_SecondsSpecified(const bSecondsSpecified:WordBool); safecall;
   function Get_Microseconds : Integer; safecall;
   procedure Set_Microseconds(const iMicroseconds:Integer); safecall;
   function Get_MicrosecondsSpecified : WordBool; safecall;
   procedure Set_MicrosecondsSpecified(const bMicrosecondsSpecified:WordBool); safecall;
   function Get_UTC : Integer; safecall;
   procedure Set_UTC(const iUTC:Integer); safecall;
   function Get_UTCSpecified : WordBool; safecall;
   procedure Set_UTCSpecified(const bUTCSpecified:WordBool); safecall;
   function Get_IsInterval : WordBool; safecall;
   procedure Set_IsInterval(const bIsInterval:WordBool); safecall;
    // GetVarDate : Retrieve value in Variant compatible (VT_DATE) format 
   function GetVarDate(bIsLocal:WordBool):TDateTime;safecall;
    // SetVarDate : Set the value using Variant compatible (VT_DATE) format 
   procedure SetVarDate(dVarDate:TDateTime;bIsLocal:WordBool);safecall;
    // GetFileTime : Retrieve value in FILETIME compatible string representation 
   function GetFileTime(bIsLocal:WordBool):WideString;safecall;
    // SetFileTime : Set the value using FILETIME compatible string representation 
   procedure SetFileTime(strFileTime:WideString;bIsLocal:WordBool);safecall;
    // Value : The DMTF datetime 
   property Value:WideString read Get_Value write Set_Value;
    // Year : The Year component of the value (must be in the range 0-9999) 
   property Year:Integer read Get_Year write Set_Year;
    // YearSpecified : Whether the Year component is specified 
   property YearSpecified:WordBool read Get_YearSpecified write Set_YearSpecified;
    // Month : The Month component of the value (must be in the range 1-12) 
   property Month:Integer read Get_Month write Set_Month;
    // MonthSpecified : Whether the Month component is specified 
   property MonthSpecified:WordBool read Get_MonthSpecified write Set_MonthSpecified;
    // Day : The Day component of the value (must be in the range 1-31, or 0-999999 for interval values) 
   property Day:Integer read Get_Day write Set_Day;
    // DaySpecified : Whether the Day component is specified 
   property DaySpecified:WordBool read Get_DaySpecified write Set_DaySpecified;
    // Hours : The Hours component of the value (must be in the range 0-23) 
   property Hours:Integer read Get_Hours write Set_Hours;
    // HoursSpecified : Whether the Hours component is specified 
   property HoursSpecified:WordBool read Get_HoursSpecified write Set_HoursSpecified;
    // Minutes : The Minutes component of the value (must be in the range 0-59) 
   property Minutes:Integer read Get_Minutes write Set_Minutes;
    // MinutesSpecified : Whether the Minutes component is specified 
   property MinutesSpecified:WordBool read Get_MinutesSpecified write Set_MinutesSpecified;
    // Seconds : The Seconds component of the value (must be in the range 0-59) 
   property Seconds:Integer read Get_Seconds write Set_Seconds;
    // SecondsSpecified : Whether the Seconds component is specified 
   property SecondsSpecified:WordBool read Get_SecondsSpecified write Set_SecondsSpecified;
    // Microseconds : The Microseconds component of the value (must be in the range 0-999999) 
   property Microseconds:Integer read Get_Microseconds write Set_Microseconds;
    // MicrosecondsSpecified : Whether the Microseconds component is specified 
   property MicrosecondsSpecified:WordBool read Get_MicrosecondsSpecified write Set_MicrosecondsSpecified;
    // UTC : The UTC component of the value (must be in the range -720 to 720) 
   property UTC:Integer read Get_UTC write Set_UTC;
    // UTCSpecified : Whether the UTC component is specified 
   property UTCSpecified:WordBool read Get_UTCSpecified write Set_UTCSpecified;
    // IsInterval : Indicates whether this value describes an absolute date and time or is an interval 
   property IsInterval:WordBool read Get_IsInterval write Set_IsInterval;
  end;


// ISWbemDateTime : A Datetime

 ISWbemDateTimeDisp = dispinterface
   ['{5E97458A-CF77-11D3-B38F-00105A1F473A}']
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
    // GetVarDate : Retrieve value in Variant compatible (VT_DATE) format 
   function GetVarDate(bIsLocal:WordBool):TDateTime;dispid 18;
    // SetVarDate : Set the value using Variant compatible (VT_DATE) format 
   procedure SetVarDate(dVarDate:TDateTime;bIsLocal:WordBool);dispid 19;
    // GetFileTime : Retrieve value in FILETIME compatible string representation 
   function GetFileTime(bIsLocal:WordBool):WideString;dispid 20;
    // SetFileTime : Set the value using FILETIME compatible string representation 
   procedure SetFileTime(strFileTime:WideString;bIsLocal:WordBool);dispid 21;
    // Value : The DMTF datetime 
   property Value:WideString dispid 0;
    // Year : The Year component of the value (must be in the range 0-9999) 
   property Year:Integer dispid 1;
    // YearSpecified : Whether the Year component is specified 
   property YearSpecified:WordBool dispid 2;
    // Month : The Month component of the value (must be in the range 1-12) 
   property Month:Integer dispid 3;
    // MonthSpecified : Whether the Month component is specified 
   property MonthSpecified:WordBool dispid 4;
    // Day : The Day component of the value (must be in the range 1-31, or 0-999999 for interval values) 
   property Day:Integer dispid 5;
    // DaySpecified : Whether the Day component is specified 
   property DaySpecified:WordBool dispid 6;
    // Hours : The Hours component of the value (must be in the range 0-23) 
   property Hours:Integer dispid 7;
    // HoursSpecified : Whether the Hours component is specified 
   property HoursSpecified:WordBool dispid 8;
    // Minutes : The Minutes component of the value (must be in the range 0-59) 
   property Minutes:Integer dispid 9;
    // MinutesSpecified : Whether the Minutes component is specified 
   property MinutesSpecified:WordBool dispid 10;
    // Seconds : The Seconds component of the value (must be in the range 0-59) 
   property Seconds:Integer dispid 11;
    // SecondsSpecified : Whether the Seconds component is specified 
   property SecondsSpecified:WordBool dispid 12;
    // Microseconds : The Microseconds component of the value (must be in the range 0-999999) 
   property Microseconds:Integer dispid 13;
    // MicrosecondsSpecified : Whether the Microseconds component is specified 
   property MicrosecondsSpecified:WordBool dispid 14;
    // UTC : The UTC component of the value (must be in the range -720 to 720) 
   property UTC:Integer dispid 15;
    // UTCSpecified : Whether the UTC component is specified 
   property UTCSpecified:WordBool dispid 16;
    // IsInterval : Indicates whether this value describes an absolute date and time or is an interval 
   property IsInterval:WordBool dispid 17;
  end;


// ISWbemRefresher : A Collection of Refreshable Objects

 ISWbemRefresher = interface(IDispatch)
   ['{14D8250E-D9C2-11D3-B38F-00105A1F473A}']
   function Get__NewEnum : IUnknown; safecall;
    // Item : Get an item from this refresher 
   function Item(iIndex:Integer):ISWbemRefreshableItem;safecall;
   function Get_Count : Integer; safecall;
    // Add : Add a refreshable instance to this refresher 
   function Add(objWbemServices:ISWbemServicesEx;bsInstancePath:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemRefreshableItem;safecall;
    // AddEnum : Add a refreshable enumerator to this refresher 
   function AddEnum(objWbemServices:ISWbemServicesEx;bsClassName:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemRefreshableItem;safecall;
    // Remove : Remove an item from this refresher 
   procedure Remove(iIndex:Integer;iFlags:Integer);safecall;
    // Refresh : Refresh all items in this collection 
   procedure Refresh(iFlags:Integer);safecall;
   function Get_AutoReconnect : WordBool; safecall;
   procedure Set_AutoReconnect(const bCount:WordBool); safecall;
    // DeleteAll : Delete all items in this collection 
   procedure DeleteAll;safecall;
    // _NewEnum :  
   property _NewEnum:IUnknown read Get__NewEnum;
    // Count : The number of items in this refresher 
   property Count:Integer read Get_Count;
    // AutoReconnect : Whether to attempt auto-reconnection to a remote provider 
   property AutoReconnect:WordBool read Get_AutoReconnect write Set_AutoReconnect;
  end;


// ISWbemRefresher : A Collection of Refreshable Objects

 ISWbemRefresherDisp = dispinterface
   ['{14D8250E-D9C2-11D3-B38F-00105A1F473A}']
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
    // Item : Get an item from this refresher 
   function Item(iIndex:Integer):ISWbemRefreshableItem;dispid 0;
    // Add : Add a refreshable instance to this refresher 
   function Add(objWbemServices:ISWbemServicesEx;bsInstancePath:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemRefreshableItem;dispid 2;
    // AddEnum : Add a refreshable enumerator to this refresher 
   function AddEnum(objWbemServices:ISWbemServicesEx;bsClassName:WideString;iFlags:Integer;objWbemNamedValueSet:IDispatch):ISWbemRefreshableItem;dispid 3;
    // Remove : Remove an item from this refresher 
   procedure Remove(iIndex:Integer;iFlags:Integer);dispid 4;
    // Refresh : Refresh all items in this collection 
   procedure Refresh(iFlags:Integer);dispid 5;
    // DeleteAll : Delete all items in this collection 
   procedure DeleteAll;dispid 7;
    // _NewEnum :  
   property _NewEnum:IUnknown  readonly dispid -4;
    // Count : The number of items in this refresher 
   property Count:Integer  readonly dispid 1;
    // AutoReconnect : Whether to attempt auto-reconnection to a remote provider 
   property AutoReconnect:WordBool dispid 6;
  end;


// ISWbemRefreshableItem : A single item in a Refresher

 ISWbemRefreshableItem = interface(IDispatch)
   ['{5AD4BF92-DAAB-11D3-B38F-00105A1F473A}']
   function Get_Index : Integer; safecall;
   function Get_Refresher : ISWbemRefresher; safecall;
   function Get_IsSet : WordBool; safecall;
   function Get_Object_ : ISWbemObjectEx; safecall;
   function Get_ObjectSet : ISWbemObjectSet; safecall;
    // Remove : Remove this item from the parent refresher 
   procedure Remove(iFlags:Integer);safecall;
    // Index : The index of this item in the parent refresher 
   property Index:Integer read Get_Index;
    // Refresher : The parent refresher 
   property Refresher:ISWbemRefresher read Get_Refresher;
    // IsSet : Whether this item represents a single object or an object set 
   property IsSet:WordBool read Get_IsSet;
    // Object : The object 
   property Object_:ISWbemObjectEx read Get_Object_;
    // ObjectSet : The object set 
   property ObjectSet:ISWbemObjectSet read Get_ObjectSet;
  end;


// ISWbemRefreshableItem : A single item in a Refresher

 ISWbemRefreshableItemDisp = dispinterface
   ['{5AD4BF92-DAAB-11D3-B38F-00105A1F473A}']
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
    // Remove : Remove this item from the parent refresher 
   procedure Remove(iFlags:Integer);dispid 6;
    // Index : The index of this item in the parent refresher 
   property Index:Integer  readonly dispid 1;
    // Refresher : The parent refresher 
   property Refresher:ISWbemRefresher  readonly dispid 2;
    // IsSet : Whether this item represents a single object or an object set 
   property IsSet:WordBool  readonly dispid 3;
    // Object : The object 
   property Object_:ISWbemObjectEx  readonly dispid 4;
    // ObjectSet : The object set 
   property ObjectSet:ISWbemObjectSet  readonly dispid 5;
  end;

//CoClasses
  CoSWbemLocator = Class
  Public
    Class Function Create: ISWbemLocator;
    Class Function CreateRemote(const MachineName: string): ISWbemLocator;
  end;

  CoSWbemNamedValueSet = Class
  Public
    Class Function Create: ISWbemNamedValueSet;
    Class Function CreateRemote(const MachineName: string): ISWbemNamedValueSet;
  end;

  CoSWbemObjectPath = Class
  Public
    Class Function Create: ISWbemObjectPath;
    Class Function CreateRemote(const MachineName: string): ISWbemObjectPath;
  end;

  CoSWbemLastError = Class
  Public
    Class Function Create: ISWbemLastError;
    Class Function CreateRemote(const MachineName: string): ISWbemLastError;
  end;

  TISWbemSinkEventsOnObjectReady = procedure(Sender: TObject;objWbemObject:ISWbemObject;objWbemAsyncContext:ISWbemNamedValueSet) of object;
  TISWbemSinkEventsOnCompleted = procedure(Sender: TObject;iHResult:WbemErrorEnum;objWbemErrorObject:ISWbemObject;objWbemAsyncContext:ISWbemNamedValueSet) of object;
  TISWbemSinkEventsOnProgress = procedure(Sender: TObject;iUpperBound:Integer;iCurrent:Integer;strMessage:WideString;objWbemAsyncContext:ISWbemNamedValueSet) of object;
  TISWbemSinkEventsOnObjectPut = procedure(Sender: TObject;objWbemObjectPath:ISWbemObjectPath;objWbemAsyncContext:ISWbemNamedValueSet) of object;


  CoSWbemSink = Class
  Public
    Class Function Create: ISWbemSink;
    Class Function CreateRemote(const MachineName: string): ISWbemSink;
  end;

  TEvsSWbemSink = Class(TEventSink)
  Private
    FOnOnObjectReady:TISWbemSinkEventsOnObjectReady;
    FOnOnCompleted:TISWbemSinkEventsOnCompleted;
    FOnOnProgress:TISWbemSinkEventsOnProgress;
    FOnOnObjectPut:TISWbemSinkEventsOnObjectPut;

    fServer:ISWbemSink;
    procedure EventSinkInvoke(Sender: TObject; DispID: Integer;
          const IID: TGUID; LocaleID: Integer; Flags: Word;
          Params: tagDISPPARAMS; VarResult, ExcepInfo, ArgErr: Pointer);
  Public
    constructor Create(TheOwner: TComponent); override;
    property ComServer:ISWbemSink read fServer;
    property OnOnObjectReady : TISWbemSinkEventsOnObjectReady read FOnOnObjectReady write FOnOnObjectReady;
    property OnOnCompleted : TISWbemSinkEventsOnCompleted read FOnOnCompleted write FOnOnCompleted;
    property OnOnProgress : TISWbemSinkEventsOnProgress read FOnOnProgress write FOnOnProgress;
    property OnOnObjectPut : TISWbemSinkEventsOnObjectPut read FOnOnObjectPut write FOnOnObjectPut;

  end;

  CoSWbemDateTime = Class
  Public
    Class Function Create: ISWbemDateTime;
    Class Function CreateRemote(const MachineName: string): ISWbemDateTime;
  end;

  CoSWbemRefresher = Class
  Public
    Class Function Create: ISWbemRefresher;
    Class Function CreateRemote(const MachineName: string): ISWbemRefresher;
  end;

  CoSWbemServices = Class
  Public
    Class Function Create: ISWbemServices;
    Class Function CreateRemote(const MachineName: string): ISWbemServices;
  end;

  CoSWbemServicesEx = Class
  Public
    Class Function Create: ISWbemServicesEx;
    Class Function CreateRemote(const MachineName: string): ISWbemServicesEx;
  end;

  CoSWbemObject = Class
  Public
    Class Function Create: ISWbemObject;
    Class Function CreateRemote(const MachineName: string): ISWbemObject;
  end;

  CoSWbemObjectEx = Class
  Public
    Class Function Create: ISWbemObjectEx;
    Class Function CreateRemote(const MachineName: string): ISWbemObjectEx;
  end;

  CoSWbemObjectSet = Class
  Public
    Class Function Create: ISWbemObjectSet;
    Class Function CreateRemote(const MachineName: string): ISWbemObjectSet;
  end;

  CoSWbemNamedValue = Class
  Public
    Class Function Create: ISWbemNamedValue;
    Class Function CreateRemote(const MachineName: string): ISWbemNamedValue;
  end;

  CoSWbemQualifier = Class
  Public
    Class Function Create: ISWbemQualifier;
    Class Function CreateRemote(const MachineName: string): ISWbemQualifier;
  end;

  CoSWbemQualifierSet = Class
  Public
    Class Function Create: ISWbemQualifierSet;
    Class Function CreateRemote(const MachineName: string): ISWbemQualifierSet;
  end;

  CoSWbemProperty = Class
  Public
    Class Function Create: ISWbemProperty;
    Class Function CreateRemote(const MachineName: string): ISWbemProperty;
  end;

  CoSWbemPropertySet = Class
  Public
    Class Function Create: ISWbemPropertySet;
    Class Function CreateRemote(const MachineName: string): ISWbemPropertySet;
  end;

  CoSWbemMethod = Class
  Public
    Class Function Create: ISWbemMethod;
    Class Function CreateRemote(const MachineName: string): ISWbemMethod;
  end;

  CoSWbemMethodSet = Class
  Public
    Class Function Create: ISWbemMethodSet;
    Class Function CreateRemote(const MachineName: string): ISWbemMethodSet;
  end;

  CoSWbemEventSource = Class
  Public
    Class Function Create: ISWbemEventSource;
    Class Function CreateRemote(const MachineName: string): ISWbemEventSource;
  end;

  CoSWbemSecurity = Class
  Public
    Class Function Create: ISWbemSecurity;
    Class Function CreateRemote(const MachineName: string): ISWbemSecurity;
  end;

  CoSWbemPrivilege = Class
  Public
    Class Function Create: ISWbemPrivilege;
    Class Function CreateRemote(const MachineName: string): ISWbemPrivilege;
  end;

  CoSWbemPrivilegeSet = Class
  Public
    Class Function Create: ISWbemPrivilegeSet;
    Class Function CreateRemote(const MachineName: string): ISWbemPrivilegeSet;
  end;

  CoSWbemRefreshableItem = Class
  Public
    Class Function Create: ISWbemRefreshableItem;
    Class Function CreateRemote(const MachineName: string): ISWbemRefreshableItem;
  end;

implementation

uses comobj;

Class Function CoSWbemLocator.Create: ISWbemLocator;
begin
  Result := CreateComObject(CLASS_SWbemLocator) as ISWbemLocator;
end;

Class Function CoSWbemLocator.CreateRemote(const MachineName: string): ISWbemLocator;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SWbemLocator) as ISWbemLocator;
end;

Class Function CoSWbemNamedValueSet.Create: ISWbemNamedValueSet;
begin
  Result := CreateComObject(CLASS_SWbemNamedValueSet) as ISWbemNamedValueSet;
end;

Class Function CoSWbemNamedValueSet.CreateRemote(const MachineName: string): ISWbemNamedValueSet;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SWbemNamedValueSet) as ISWbemNamedValueSet;
end;

Class Function CoSWbemObjectPath.Create: ISWbemObjectPath;
begin
  Result := CreateComObject(CLASS_SWbemObjectPath) as ISWbemObjectPath;
end;

Class Function CoSWbemObjectPath.CreateRemote(const MachineName: string): ISWbemObjectPath;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SWbemObjectPath) as ISWbemObjectPath;
end;

Class Function CoSWbemLastError.Create: ISWbemLastError;
begin
  Result := CreateComObject(CLASS_SWbemLastError) as ISWbemLastError;
end;

Class Function CoSWbemLastError.CreateRemote(const MachineName: string): ISWbemLastError;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SWbemLastError) as ISWbemLastError;
end;

Class Function CoSWbemSink.Create: ISWbemSink;
begin
  Result := CreateComObject(CLASS_SWbemSink) as ISWbemSink;
end;

Class Function CoSWbemSink.CreateRemote(const MachineName: string): ISWbemSink;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SWbemSink) as ISWbemSink;
end;

constructor TEvsSWbemSink.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  OnInvoke:=EventSinkInvoke;
  fServer:=CoSWbemSink.Create;
  Connect(fServer,ISWbemSinkEvents);
end;

procedure TEvsSWbemSink.EventSinkInvoke(Sender: TObject; DispID: Integer;
  const IID: TGUID; LocaleID: Integer; Flags: Word; Params: tagDISPPARAMS;
  VarResult, ExcepInfo, ArgErr: Pointer);
begin
  case DispID of
    1: if assigned(OnOnObjectReady) then
          OnOnObjectReady(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    2: if assigned(OnOnCompleted) then
          OnOnCompleted(Self, OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    3: if assigned(OnOnProgress) then
          OnOnProgress(Self, OleVariant(Params.rgvarg[3]), OleVariant(Params.rgvarg[2]), OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));
    4: if assigned(OnOnObjectPut) then
          OnOnObjectPut(Self, OleVariant(Params.rgvarg[1]), OleVariant(Params.rgvarg[0]));

  end;
end;

Class Function CoSWbemDateTime.Create: ISWbemDateTime;
begin
  Result := CreateComObject(CLASS_SWbemDateTime) as ISWbemDateTime;
end;

Class Function CoSWbemDateTime.CreateRemote(const MachineName: string): ISWbemDateTime;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SWbemDateTime) as ISWbemDateTime;
end;

Class Function CoSWbemRefresher.Create: ISWbemRefresher;
begin
  Result := CreateComObject(CLASS_SWbemRefresher) as ISWbemRefresher;
end;

Class Function CoSWbemRefresher.CreateRemote(const MachineName: string): ISWbemRefresher;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SWbemRefresher) as ISWbemRefresher;
end;

Class Function CoSWbemServices.Create: ISWbemServices;
begin
  Result := CreateComObject(CLASS_SWbemServices) as ISWbemServices;
end;

Class Function CoSWbemServices.CreateRemote(const MachineName: string): ISWbemServices;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SWbemServices) as ISWbemServices;
end;

Class Function CoSWbemServicesEx.Create: ISWbemServicesEx;
begin
  Result := CreateComObject(CLASS_SWbemServicesEx) as ISWbemServicesEx;
end;

Class Function CoSWbemServicesEx.CreateRemote(const MachineName: string): ISWbemServicesEx;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SWbemServicesEx) as ISWbemServicesEx;
end;

Class Function CoSWbemObject.Create: ISWbemObject;
begin
  Result := CreateComObject(CLASS_SWbemObject) as ISWbemObject;
end;

Class Function CoSWbemObject.CreateRemote(const MachineName: string): ISWbemObject;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SWbemObject) as ISWbemObject;
end;

Class Function CoSWbemObjectEx.Create: ISWbemObjectEx;
begin
  Result := CreateComObject(CLASS_SWbemObjectEx) as ISWbemObjectEx;
end;

Class Function CoSWbemObjectEx.CreateRemote(const MachineName: string): ISWbemObjectEx;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SWbemObjectEx) as ISWbemObjectEx;
end;

Class Function CoSWbemObjectSet.Create: ISWbemObjectSet;
begin
  Result := CreateComObject(CLASS_SWbemObjectSet) as ISWbemObjectSet;
end;

Class Function CoSWbemObjectSet.CreateRemote(const MachineName: string): ISWbemObjectSet;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SWbemObjectSet) as ISWbemObjectSet;
end;

Class Function CoSWbemNamedValue.Create: ISWbemNamedValue;
begin
  Result := CreateComObject(CLASS_SWbemNamedValue) as ISWbemNamedValue;
end;

Class Function CoSWbemNamedValue.CreateRemote(const MachineName: string): ISWbemNamedValue;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SWbemNamedValue) as ISWbemNamedValue;
end;

Class Function CoSWbemQualifier.Create: ISWbemQualifier;
begin
  Result := CreateComObject(CLASS_SWbemQualifier) as ISWbemQualifier;
end;

Class Function CoSWbemQualifier.CreateRemote(const MachineName: string): ISWbemQualifier;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SWbemQualifier) as ISWbemQualifier;
end;

Class Function CoSWbemQualifierSet.Create: ISWbemQualifierSet;
begin
  Result := CreateComObject(CLASS_SWbemQualifierSet) as ISWbemQualifierSet;
end;

Class Function CoSWbemQualifierSet.CreateRemote(const MachineName: string): ISWbemQualifierSet;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SWbemQualifierSet) as ISWbemQualifierSet;
end;

Class Function CoSWbemProperty.Create: ISWbemProperty;
begin
  Result := CreateComObject(CLASS_SWbemProperty) as ISWbemProperty;
end;

Class Function CoSWbemProperty.CreateRemote(const MachineName: string): ISWbemProperty;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SWbemProperty) as ISWbemProperty;
end;

Class Function CoSWbemPropertySet.Create: ISWbemPropertySet;
begin
  Result := CreateComObject(CLASS_SWbemPropertySet) as ISWbemPropertySet;
end;

Class Function CoSWbemPropertySet.CreateRemote(const MachineName: string): ISWbemPropertySet;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SWbemPropertySet) as ISWbemPropertySet;
end;

Class Function CoSWbemMethod.Create: ISWbemMethod;
begin
  Result := CreateComObject(CLASS_SWbemMethod) as ISWbemMethod;
end;

Class Function CoSWbemMethod.CreateRemote(const MachineName: string): ISWbemMethod;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SWbemMethod) as ISWbemMethod;
end;

Class Function CoSWbemMethodSet.Create: ISWbemMethodSet;
begin
  Result := CreateComObject(CLASS_SWbemMethodSet) as ISWbemMethodSet;
end;

Class Function CoSWbemMethodSet.CreateRemote(const MachineName: string): ISWbemMethodSet;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SWbemMethodSet) as ISWbemMethodSet;
end;

Class Function CoSWbemEventSource.Create: ISWbemEventSource;
begin
  Result := CreateComObject(CLASS_SWbemEventSource) as ISWbemEventSource;
end;

Class Function CoSWbemEventSource.CreateRemote(const MachineName: string): ISWbemEventSource;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SWbemEventSource) as ISWbemEventSource;
end;

Class Function CoSWbemSecurity.Create: ISWbemSecurity;
begin
  Result := CreateComObject(CLASS_SWbemSecurity) as ISWbemSecurity;
end;

Class Function CoSWbemSecurity.CreateRemote(const MachineName: string): ISWbemSecurity;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SWbemSecurity) as ISWbemSecurity;
end;

Class Function CoSWbemPrivilege.Create: ISWbemPrivilege;
begin
  Result := CreateComObject(CLASS_SWbemPrivilege) as ISWbemPrivilege;
end;

Class Function CoSWbemPrivilege.CreateRemote(const MachineName: string): ISWbemPrivilege;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SWbemPrivilege) as ISWbemPrivilege;
end;

Class Function CoSWbemPrivilegeSet.Create: ISWbemPrivilegeSet;
begin
  Result := CreateComObject(CLASS_SWbemPrivilegeSet) as ISWbemPrivilegeSet;
end;

Class Function CoSWbemPrivilegeSet.CreateRemote(const MachineName: string): ISWbemPrivilegeSet;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SWbemPrivilegeSet) as ISWbemPrivilegeSet;
end;

Class Function CoSWbemRefreshableItem.Create: ISWbemRefreshableItem;
begin
  Result := CreateComObject(CLASS_SWbemRefreshableItem) as ISWbemRefreshableItem;
end;

Class Function CoSWbemRefreshableItem.CreateRemote(const MachineName: string): ISWbemRefreshableItem;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_SWbemRefreshableItem) as ISWbemRefreshableItem;
end;

end.
