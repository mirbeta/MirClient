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

unit dxRichEdit.Dialogs.RangeEditingPermissionsFormController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCoreClasses,

  dxRichEdit.Dialogs.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.ProtectionFormatting,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.View.Core,
  dxRichEdit.Utils.Properties,
  dxRichEdit.Utils.Types,
  dxGenerics;

type
  IdxUserListService = interface
  ['{B69393A7-9A1E-4E44-B366-C1A1C13EB590}']
    function GetUsers: TStrings;
  end;

  IdxUserGroupListService = interface
  ['{3674A2FC-7A08-4ECC-A85E-34C0B9626CE8}']
    function GetUserGroups: TStrings;
  end;

  { TdxRangeEditingPermissionsFormControllerParameters }

  TdxRangeEditingPermissionsFormControllerParameters = class(TdxFormControllerParameters)
  end;

  { TdxRangeEditingPermissionsFormController }

  TdxRangeEditingPermissionsFormController = class(TdxFormController)
  private
    const
      EmailPattern = '^([a-z0-9_-]+\.)*[a-z0-9_-]+@[a-z0-9_-]+(\.[a-z0-9_-]+)*\.[a-z]{2,6}$';
  strict private
    FControl: IdxRichEditControl;
    FCheckedUsers: TStrings;
    FCheckedUserGroups: TStrings;
    FAvailableUsers: TStrings;
    FAvailableUserGroups: TStrings;
    FRangePermissions: TdxRangePermissionCollection;
    FRangePermissionsWithSelection: TdxRangePermissionCollection;
    FInternalUsers: TStrings;
    FInternalUserGroups: TStrings;
  protected
    procedure Initialize; virtual;
    function ObtainAvailableUsers: TStrings;
    function ObtainAvailableUserGroups: TStrings;
    function ObtainCheckedUsers(ARangePermissions: TdxRangePermissionCollection): TStrings; virtual;
    function ObtainCheckedGroups(ARangePermissions: TdxRangePermissionCollection): TStrings; virtual;
    procedure MergeStringLists(const ATarget, AListToAppend: TStrings);
    function ObtainRangePermissionsMatchSelection: TdxRangePermissionCollection; virtual;
    function ObtainRangePermissionsWithSelectionInside: TdxRangePermissionCollection; virtual;
    procedure RemoveOldRangePermissions(APieceTable: TdxPieceTable);
    procedure ApplyNewRangePermissions(APieceTable: TdxPieceTable); overload;
    procedure ApplyNewRangePermissions(ASelectionItems: TdxSelectionItemList; APermissions: TdxRangePermissionInfoList); overload;
    procedure ApplyNewRangePermissions(AItem: TdxSelectionItem; APermissions: TdxRangePermissionInfoList); overload;
    procedure RemoveRangePermissions(ASelectionItems: TdxSelectionItemList); overload;
    procedure RemoveRangePermissions(AItem: TdxSelectionItem); overload;
    procedure AppendUserGroupNamePermissions(APermissions: TdxRangePermissionInfoList);
    procedure AppendUserNamePermissions(APermissions: TdxRangePermissionInfoList);

    property Control: IdxRichEditControl read FControl;
    property RangePermissions: TdxRangePermissionCollection read FRangePermissions;
  public
    constructor Create(AControllerParameters: TdxRangeEditingPermissionsFormControllerParameters);
    destructor Destroy; override;
    procedure ApplyChanges; override;
    function AddUser(const AUserName: string): Boolean;
    procedure CheckUser(const AUserName: string);
    procedure CheckUserGroup(const AUserGroupName: string);
    function HasUserListService: Boolean;
    procedure UncheckUser(const AUserName: string);
    procedure UncheckUserGroup(const AUserGroupName: string);
    function ValidNserName(const AUserName: string): Boolean;

    property CheckedUsers: TStrings read FCheckedUsers;
    property CheckedUserGroups: TStrings read FCheckedUserGroups;
    property AvailableUsers: TStrings read FAvailableUsers;
    property AvailableUserGroups: TStrings read FAvailableUserGroups;
  end;

implementation

uses
  RTLConsts, Contnrs, Math, RegularExpressions;

type
  TStringsHelper = class helper for TStrings
  public
    function Contains(const AValue: string): Boolean;
    function Remove(const AValue: string): Integer;
  end;

function TStringsHelper.Contains(const AValue: string): Boolean;
begin
  Result := IndexOf(AValue) >= 0;
end;

function TStringsHelper.Remove(const AValue: string): Integer;
begin
  Result := IndexOf(AValue);
  if Result >= 0 then
    Delete(Result);
end;

{ TdxRangeEditingPermissionsFormController }

constructor TdxRangeEditingPermissionsFormController.Create(AControllerParameters: TdxRangeEditingPermissionsFormControllerParameters);
begin
  inherited Create;
  Assert(AControllerParameters <> nil, 'controllerParameters');
  FControl := AControllerParameters.Control;

  Initialize;
end;

destructor TdxRangeEditingPermissionsFormController.Destroy;
begin
  FreeAndNil(FInternalUsers);
  FreeAndNil(FInternalUserGroups);
  FreeAndNil(FCheckedUsers);
  FreeAndNil(FCheckedUserGroups);
  FreeAndNil(FRangePermissions);
  FreeAndNil(FRangePermissionsWithSelection);
  inherited Destroy;
end;

function TdxRangeEditingPermissionsFormController.HasUserListService: Boolean;
var
  AService: IdxUserListService;
begin
  AService := FControl.InnerControl.DocumentModel.GetService<IdxUserListService>;
  Result := AService <> nil;
end;

procedure TdxRangeEditingPermissionsFormController.Initialize;
var
  APermissions: TdxRangePermissionCollection;
begin
  FRangePermissions := ObtainRangePermissionsMatchSelection;

  if FRangePermissions.Count > 0 then
    APermissions := FRangePermissions
  else
  begin
    FRangePermissionsWithSelection := ObtainRangePermissionsWithSelectionInside;
    APermissions := FRangePermissionsWithSelection;
  end;
  FAvailableUsers := ObtainAvailableUsers;
  FAvailableUserGroups := ObtainAvailableUserGroups;
  FCheckedUsers := ObtainCheckedUsers(APermissions);
  FCheckedUserGroups := ObtainCheckedGroups(APermissions);
  MergeStringLists(FAvailableUsers, FCheckedUsers);
  MergeStringLists(FAvailableUserGroups, FCheckedUserGroups);
end;

procedure TdxRangeEditingPermissionsFormController.CheckUser(const AUserName: string);
begin
  if AUserName = '' then
    Exit;

  if not CheckedUsers.Contains(AUserName) then
    CheckedUsers.Add(AUserName);
end;

procedure TdxRangeEditingPermissionsFormController.UncheckUser(const AUserName: string);
begin
  if AUserName = '' then
    Exit;

  CheckedUsers.Remove(AUserName);
end;

procedure TdxRangeEditingPermissionsFormController.CheckUserGroup(const AUserGroupName: string);
begin
  if not CheckedUserGroups.Contains(AUserGroupName) then
    CheckedUserGroups.Add(AUserGroupName);
end;

procedure TdxRangeEditingPermissionsFormController.UncheckUserGroup(const AUserGroupName: string);
begin
  CheckedUserGroups.Remove(AUserGroupName);
end;

function TdxRangeEditingPermissionsFormController.ValidNserName(const AUserName: string): Boolean;
var
  ARegEx: TRegEx;
begin
  ARegEx := TRegEx.Create(EmailPattern);
  Result := ARegEx.IsMatch(AUserName);
end;

function TdxRangeEditingPermissionsFormController.ObtainAvailableUsers: TStrings;
var
  AService: IdxUserListService;
  I: Integer;
  ARangePermissionInfo: TdxRangePermissionInfo;
begin
  AService := FControl.InnerControl.DocumentModel.GetService<IdxUserListService>;
  if AService <> nil then
    Result := AService.GetUsers
  else
  begin
    if FInternalUsers = nil then
    begin
      FInternalUsers := TStringList.Create;
      for I := 0 to Control.DocumentModel.Cache.RangePermissionInfoCache.Count - 1 do
      begin
        ARangePermissionInfo := Control.DocumentModel.Cache.RangePermissionInfoCache.Items[I];
        if ARangePermissionInfo.UserName <> '' then
          FInternalUsers.Add(ARangePermissionInfo.UserName);
      end;
    end;
    Result := FInternalUsers;
  end;
end;

function TdxRangeEditingPermissionsFormController.ObtainAvailableUserGroups: TStrings;
var
  AService: IdxUserGroupListService;
  I: Integer;
  ARangePermissionInfo: TdxRangePermissionInfo;
begin
  AService := FControl.InnerControl.DocumentModel.GetService<IdxUserGroupListService>;
  if AService <> nil then
    Result := AService.GetUserGroups
  else
  begin
    if FInternalUserGroups = nil then
    begin
      FInternalUserGroups := TStringList.Create;
      for I := 0 to Control.DocumentModel.Cache.RangePermissionInfoCache.Count - 1 do
      begin
        ARangePermissionInfo := Control.DocumentModel.Cache.RangePermissionInfoCache.Items[I];
        if (ARangePermissionInfo.Group <> '') and not FInternalUserGroups.Contains(ARangePermissionInfo.Group) then
          FInternalUserGroups.Add(ARangePermissionInfo.Group);
      end;
    end;
    Result := FInternalUserGroups;
  end;
end;

function TdxRangeEditingPermissionsFormController.ObtainCheckedUsers(ARangePermissions: TdxRangePermissionCollection): TStrings;
var
  ACount, I: Integer;
  AValue: string;
begin
  Result := TStringList.Create;
  ACount := ARangePermissions.Count;
  for I := 0 to ACount - 1 do
  begin
    AValue := ARangePermissions[I].UserName;
    if (AValue <> '') and not Result.Contains(AValue) then
      Result.Add(AValue);
  end;
end;

function TdxRangeEditingPermissionsFormController.ObtainCheckedGroups(ARangePermissions: TdxRangePermissionCollection): TStrings;
var
  ACount, I: Integer;
  AValue: string;
begin
  Result := TStringList.Create;
  ACount := ARangePermissions.Count;
  for I := 0 to ACount - 1 do
  begin
    AValue := ARangePermissions[I].Group;
    if (AValue <> '') and not Result.Contains(AValue) then
      Result.Add(AValue);
  end;
end;

procedure TdxRangeEditingPermissionsFormController.MergeStringLists(const ATarget, AListToAppend: TStrings);
var
  ACount, I: Integer;
  AValue: string;
begin
  ACount := AListToAppend.Count;
  for I := 0 to ACount - 1 do
  begin
    AValue := AListToAppend[I];
    if not ATarget.Contains(AValue) then
      ATarget.Add(AValue);
  end;
end;

function TdxRangeEditingPermissionsFormController.ObtainRangePermissionsMatchSelection: TdxRangePermissionCollection;
var
  AInnerControl: IdxInnerControl;
  ASelection: TdxSelection;
begin
  AInnerControl := FControl.InnerControl;
  ASelection := AInnerControl.DocumentModel.Selection;
  Result := ASelection.PieceTable.ObtainRangePermissionsMatchSelection;
end;

function TdxRangeEditingPermissionsFormController.ObtainRangePermissionsWithSelectionInside: TdxRangePermissionCollection;
var
  AInnerControl: IdxInnerControl;
  ASelection: TdxSelection;
  APermissions: TdxRangePermissionCollection;
  ACount, AIndex, I: Integer;
begin
  AInnerControl := FControl.InnerControl;
  ASelection := AInnerControl.DocumentModel.Selection;
  APermissions := ASelection.PieceTable.ObtainRangePermissionsWithSelectionInside;
  ACount := APermissions.Count;
  if ACount <= 0 then
    Exit(APermissions);

  if ASelection.Items.Count > 1 then
  begin
    AIndex := APermissions[0].Properties.Index;
    for I := 1 to ACount - 1 do
    begin
      if APermissions[I].Properties.Index <> AIndex then
      begin
        APermissions.Clear;
        Break;
      end;
    end;
  end;
  Result := APermissions;
end;

procedure TdxRangeEditingPermissionsFormController.ApplyChanges;
var
  ADocumentModel: TdxDocumentModel;
  APieceTable: TdxPieceTable;
begin
  ADocumentModel := FControl.InnerControl.DocumentModel;
  FControl.BeginUpdate;
  try
    ADocumentModel.BeginUpdate;
    try
      APieceTable := ADocumentModel.ActivePieceTable;
      RemoveOldRangePermissions(APieceTable);
      ApplyNewRangePermissions(APieceTable);
    finally
      ADocumentModel.EndUpdate;
    end;
  finally
    FControl.EndUpdate;
  end;
end;

procedure TdxRangeEditingPermissionsFormController.RemoveOldRangePermissions(APieceTable: TdxPieceTable);
var
  ACount, I: Integer;
begin
  ACount := FRangePermissions.Count;
  for I := 0 to ACount - 1 do
    APieceTable.DeleteRangePermission(FRangePermissions[I]);
end;

procedure TdxRangeEditingPermissionsFormController.ApplyNewRangePermissions(APieceTable: TdxPieceTable);
var
  APermissions: TdxRangePermissionInfoList;
begin
  APermissions := TdxRangePermissionInfoList.Create(True);
  try
    AppendUserNamePermissions(APermissions);
    AppendUserGroupNamePermissions(APermissions);
    if APermissions.Count > 0 then
      ApplyNewRangePermissions(APieceTable.DocumentModel.Selection.Items, APermissions)
    else
      RemoveRangePermissions(APieceTable.DocumentModel.Selection.Items);
  finally
    APermissions.Free;
  end;
end;

procedure TdxRangeEditingPermissionsFormController.ApplyNewRangePermissions(ASelectionItems: TdxSelectionItemList;
  APermissions: TdxRangePermissionInfoList);
var
  ACount, I: Integer;
begin
  ACount := ASelectionItems.Count;
  for I := 0 to ACount - 1 do
    ApplyNewRangePermissions(ASelectionItems[I], APermissions);
end;

procedure TdxRangeEditingPermissionsFormController.ApplyNewRangePermissions(AItem: TdxSelectionItem;
  APermissions: TdxRangePermissionInfoList);
var
  ACount, I: Integer;
begin
  ACount := APermissions.Count;
  for I := 0 to ACount - 1 do
    TdxPieceTable(AItem.PieceTable).ApplyDocumentPermission(AItem.NormalizedStart, AItem.NormalizedEnd, APermissions[I]);
end;

procedure TdxRangeEditingPermissionsFormController.RemoveRangePermissions(ASelectionItems: TdxSelectionItemList);
var
  ACount, I: Integer;
begin
  ACount := ASelectionItems.Count;
  for I := 0 to ACount - 1 do
    RemoveRangePermissions(ASelectionItems[I]);
end;

procedure TdxRangeEditingPermissionsFormController.RemoveRangePermissions(AItem: TdxSelectionItem);
var
  APermissions: TdxRangePermissionCollection;
  I: Integer;
  APermission: TdxRangePermission;
  AStart, AEnd: TdxDocumentLogPosition;
begin
  APermissions := TdxPieceTable(AItem.PieceTable).RangePermissions;

  I := 0;
  while I < APermissions.Count do
  begin
    APermission := APermissions[I];
    if APermission.IntersectsWithExcludingBounds(AItem) then
    begin
      if AItem.Length <> 0 then
        AStart := Max(AItem.NormalizedStart, APermission.NormalizedStart)
      else
        AStart := APermission.NormalizedStart;
      if AItem.Length <> 0 then
        AEnd := Min(AItem.NormalizedEnd, APermission.NormalizedEnd)
      else
        AEnd := APermission.NormalizedEnd;
      TdxPieceTable(AItem.PieceTable).RemoveDocumentPermission(AStart, AEnd, APermission.Properties.Info);
      I := 0;
      Continue;
    end;
    Inc(I);
  end;
end;

function TdxRangeEditingPermissionsFormController.AddUser(const AUserName: string): Boolean;
begin
  Result := False;
  if AUserName = '' then
    Exit;
  if ValidNserName(AUserName) and not AvailableUsers.Contains(AUserName) then
  begin
    AvailableUsers.Add(AUserName);
    Result := True;
  end;
end;

procedure TdxRangeEditingPermissionsFormController.AppendUserGroupNamePermissions(APermissions: TdxRangePermissionInfoList);
var
  ACount, I: Integer;
  AInfo: TdxRangePermissionInfo;
begin
  ACount := CheckedUsers.Count;
  for I := 0 to ACount - 1 do
  begin
    AInfo := TdxRangePermissionInfo.Create;
    AInfo.UserName := CheckedUsers[I];
    APermissions.Add(AInfo);
  end;
end;

procedure TdxRangeEditingPermissionsFormController.AppendUserNamePermissions(APermissions: TdxRangePermissionInfoList);
var
  ACount, I: Integer;
  AInfo: TdxRangePermissionInfo;
begin
  ACount := CheckedUserGroups.Count;
  for I := 0 to ACount - 1 do
  begin
    AInfo := TdxRangePermissionInfo.Create;
    AInfo.Group := CheckedUserGroups[I];
    APermissions.Add(AInfo);
  end;
end;

end.

