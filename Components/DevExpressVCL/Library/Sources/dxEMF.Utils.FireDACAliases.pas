{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEntityMapping Framework                           }
{                                                                    }
{           Copyright (c) 2016-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, DPU, SO, ETC.) ARE CONFIDENTIAL AND PROPRIETARY  }
{   TRADE SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER }
{   IS LICENSED TO DISTRIBUTE THE EXPRESSENTITYMAPPING FRAMEWORK     }
{   AS PART OF AN EXECUTABLE PROGRAM ONLY.                           }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxEMF.Utils.FireDACAliases;

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
{$IFDEF DELPHIXE5}
  FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.Stan.Def, FireDAC.Stan.Intf, FireDAC.Phys.Intf, FireDAC.Comp.Script;
{$ELSE}
  uADCompClient, uADStanParam, uADStanDef, uADStanOption, uADStanIntf, uADPhysIntf, uADCompScript;
{$ENDIF}

type
{$IFNDEF DELPHIXE5}
  TFDManager = TADManager;
  TFDQuery = TADQuery;
  TFDConnection = TADConnection;
  TFDTransaction = TADTransaction;
  TFDCommand = TADCommand;
  TFDCustomQuery = TADCustomQuery;
  TFDTable = TADTable;
  TFDParam = TADParam;
  TFDScript = TADScript;
  TFDPhysMetaInfoKind = TADPhysMetaInfoKind;
  TFDDataType = TADDataType;
  TFDMetaInfoQuery = TADMetaInfoQuery;
  TFDDataAttribute = TADDataAttribute;
  TFDDataAttributes = TADDataAttributes;
  TFDPhysIndexKind = TADPhysIndexKind;
  TFDPhysTableKind = TADPhysTableKind;
  TFDPhysObjectScope = TADPhysObjectScope;
{$ELSE}
  TFDManager = FireDAC.Comp.Client.TFDManager;
  TFDQuery = FireDAC.Comp.Client.TFDQuery;
  TFDConnection = FireDAC.Comp.Client.TFDConnection;
  TFDTransaction = FireDAC.Comp.Client.TFDTransaction;
  TFDCommand = FireDAC.Comp.Client.TFDCommand;
  TFDCustomQuery = FireDAC.Comp.Client.TFDCustomQuery;
  TFDTable = FireDAC.Comp.Client.TFDTable;
  TFDParam = FireDAC.Stan.Param.TFDParam;
  TFDScript = FireDAC.Comp.Script.TFDScript;
  TFDPhysMetaInfoKind = type FireDAC.Phys.Intf.TFDPhysMetaInfoKind;
  TFDDataType = type FireDAC.Stan.Intf.TFDDataType;
  TFDMetaInfoQuery = FireDAC.Comp.Client.TFDMetaInfoQuery;
  TFDDataAttribute = FireDAC.Stan.Intf.TFDDataAttribute;
  TFDDataAttributes = FireDAC.Stan.Intf.TFDDataAttributes;
  TFDPhysIndexKind = FireDAC.Phys.Intf.TFDPhysIndexKind;
  TFDPhysTableKind = FireDAC.Phys.Intf.TFDPhysTableKind;
  TFDPhysObjectScope = FireDAC.Phys.Intf.TFDPhysObjectScope;
{$ENDIF}

implementation

end.
