{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.Api.Protection;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils,

  dxRichEdit.NativeApi,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.ProtectionFormatting;

type
  { TdxNativeRangePermission }

  TdxNativeRangePermission = class(TInterfacedObject, IdxRichEditRangePermission)
  strict private
    FRange: IdxRichEditDocumentRange;
    FInfo: TdxRangePermissionInfo;
    function GetGroup: string;
    function GetRange: IdxRichEditDocumentRange;
    function GetUserName: string;
    procedure SetGroup(const Value: string);
    procedure SetUserName(const Value: string);
  public
    constructor Create(const ARange: IdxRichEditDocumentRange);
    destructor Destroy; override;

    property Range: IdxRichEditDocumentRange read FRange;
    property UserName: string read GetUserName write SetUserName;
    property Group: string read GetGroup write SetGroup;
  end;

  { TdxNativeRangePermissionCollection }

  TdxNativeRangePermissionCollection = class(TdxIUnknownList<IdxRichEditRangePermission>, IdxRichEditRangePermissionCollection)
  strict private
    function GetCount: Integer;
  public
    function CreateRangePermission(const ARange: IdxRichEditDocumentRange): IdxRichEditRangePermission;
  end;

implementation

{ TdxNativeRangePermission }

constructor TdxNativeRangePermission.Create(
  const ARange: IdxRichEditDocumentRange);
begin
  inherited Create;
  FRange := ARange;
  FInfo := TdxRangePermissionInfo.Create;
end;

destructor TdxNativeRangePermission.Destroy;
begin
  FreeAndNil(FInfo);
  inherited Destroy;
end;

function TdxNativeRangePermission.GetGroup: string;
begin
  Result := FInfo.Group;
end;

function TdxNativeRangePermission.GetRange: IdxRichEditDocumentRange;
begin
  Result := FRange;
end;

function TdxNativeRangePermission.GetUserName: string;
begin
  Result := FInfo.UserName;
end;

procedure TdxNativeRangePermission.SetGroup(const Value: string);
begin
  FInfo.Group := Value;
end;

procedure TdxNativeRangePermission.SetUserName(const Value: string);
begin
  FInfo.UserName := Value;
end;

{ TdxNativeRangePermissionCollection }

function TdxNativeRangePermissionCollection.CreateRangePermission(
  const ARange: IdxRichEditDocumentRange): IdxRichEditRangePermission;
begin
  Result := TdxNativeRangePermission.Create(ARange);
end;

function TdxNativeRangePermissionCollection.GetCount: Integer;
begin
  Result := inherited Count;
end;

end.
