{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressScheduler                                         }
{                                                                    }
{           Copyright (c) 2003-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSCHEDULER AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit cxSchedulerWebServiceStorageReg;

{$I cxVer.inc}

interface

uses
  Classes, SysUtils, Menus,
  Types, DesignIntf, DesignEditors,  DesignMenus, VCLEditors,
  cxDesignWindows, cxEditPropEditors,
  dxCoreReg, cxLibraryReg,
  cxSchedulerReg;

procedure Register;

implementation

uses
  cxSchedulerWebServiceStorage,
  cxSchedulerWebServiceStorageGoogleProvider,
  cxSchedulerWebServiceStorageOfficeProvider,
  dxAuthorizationAgents;

type
  TcxSchedulerWebServiceStorageCustomProviderAccess = class(TcxSchedulerWebServiceStorageCustomProvider);

  { TcxSchedulerWebServiceStorageSelectionEditor }

  TcxSchedulerWebServiceStorageSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { TcxSchedulerCloudStorageProviderProperty }

  TcxSchedulerWebServiceStorageProviderProperty = class(TClassProperty)
  strict private
    function GetItem: TcxSchedulerWebServiceStorageResourceItem;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TcxSchedulerCloudStorageProviderCalendarIDProperty }

  TcxSchedulerCloudStorageProviderCalendarIDProperty = class(TStringProperty)
  strict private
    function GetList: TdxWebServiceCalendarList;
    function GetProvider: TcxSchedulerWebServiceStorageCustomProvider;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

{ TcxSchedulerWebServiceStorageSelectionEditor }

procedure TcxSchedulerWebServiceStorageSelectionEditor.RequiresUnits(
  Proc: TGetStrProc);
var
  I, J: Integer;
  AStorage: TcxSchedulerWebServiceStorage;
begin
  for I := 0 to Designer.Root.ComponentCount - 1 do
  begin
    if Designer.Root.Components[I] is TcxSchedulerWebServiceStorage then
    begin
      AStorage := TcxSchedulerWebServiceStorage(Designer.Root.Components[I]);
      for J := 0 to AStorage.Resources.Items.Count - 1 do
      begin
        if AStorage.Resources.Items[J].ProviderClass <> nil then
          Proc(AStorage.Resources.Items[J].ProviderClass.UnitName);
      end;
    end;
  end;
end;

{ TcxSchedulerCloudStorageProviderProperty }

function TcxSchedulerWebServiceStorageProviderProperty.GetAttributes: TPropertyAttributes;
var
  AItem: TcxSchedulerWebServiceStorageResourceItem;
begin
  Result := inherited GetAttributes;
  AItem := GetItem;
  if (AItem <> nil) and (AItem.ProviderClass <> nil) then
    Include(Result, paSubProperties)
  else
    Exclude(Result, paSubProperties);
  Result := Result - [paReadOnly] + [paValueList, paSortList, paRevertable, paVolatileSubProperties];
end;

function TcxSchedulerWebServiceStorageProviderProperty.GetValue: string;
var
  AItem: TcxSchedulerWebServiceStorageResourceItem;
begin
  AItem := GetItem;
  if AItem <> nil then
  begin
    if AItem.ProviderClass <> nil then
      Exit(AItem.ProviderClass.GetDisplayName);
  end;
  Result := '';
end;

procedure TcxSchedulerWebServiceStorageProviderProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  inherited GetValues(Proc);
  for I := 0 to TcxSchedulerWebServiceStorage.RegisteredCalendarProviders.Count - 1 do
    Proc(TcxSchedulerWebServiceStorage.RegisteredCalendarProviders.Descriptions[I]);
end;

procedure TcxSchedulerWebServiceStorageProviderProperty.SetValue(const Value: string);
var
  AItem: TcxSchedulerWebServiceStorageResourceItem;
  AClass: TClass;
begin
  AItem := GetItem;
  if AItem <> nil then
  begin
    if Value <> '' then
      AClass := TcxSchedulerWebServiceStorage.RegisteredCalendarProviders.FindByDescription(Value)
    else
      AClass := nil;
    if AClass = nil then
        AItem.ProviderClass := nil
      else
        AItem.ProviderClass := TcxSchedulerWebServiceStorageCustomProviderClass(AClass);
    Modified;
  end;
end;

function TcxSchedulerWebServiceStorageProviderProperty.GetItem: TcxSchedulerWebServiceStorageResourceItem;
var
  I: Integer;
begin
  for I := 0 to PropCount - 1 do
    if GetComponent(I) is TcxSchedulerWebServiceStorageResourceItem then
      Exit(TcxSchedulerWebServiceStorageResourceItem(GetComponent(I)));
  Result := nil;
end;

{ TcxSchedulerCloudStorageProviderCalendarIDProperty }

function TcxSchedulerCloudStorageProviderCalendarIDProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TcxSchedulerCloudStorageProviderCalendarIDProperty.GetValues(
  Proc: TGetStrProc);
var
  AList: TdxWebServiceCalendarList;
  I: Integer;
begin
  AList := GetList;
  try
    for I := 0 to AList.Count - 1 do
      Proc(AList[I].Name);
  finally
    AList.Free;
  end;
end;

function TcxSchedulerCloudStorageProviderCalendarIDProperty.GetValue: string;
var
  AProvider: TcxSchedulerWebServiceStorageCustomProvider;
begin
  AProvider := GetProvider;
  if AProvider = nil then
    Exit('');
  if AProvider.Calendar.IsNull then
    Result := AProvider.CalendarID
  else
    Result := AProvider.Calendar.Name;
end;

procedure TcxSchedulerCloudStorageProviderCalendarIDProperty.SetValue(const Value: string);
var
  AProvider: TcxSchedulerWebServiceStorageCustomProvider;
  AList: TdxWebServiceCalendarList;
  ACalendar: TdxWebServiceCalendar;
begin
  AProvider := GetProvider;
  if AProvider = nil then
    Exit;
  AList := GetList;
  try
    ACalendar := AList.FindByName(Value);
    if ACalendar.IsNull then
      AProvider.CalendarID := Value
    else
      AProvider.CalendarID := ACalendar.ID;
  finally
    AList.Free;
  end;
end;

function TcxSchedulerCloudStorageProviderCalendarIDProperty.GetList: TdxWebServiceCalendarList;
var
  AProvider: TcxSchedulerWebServiceStorageCustomProvider;
begin
  AProvider := GetProvider;
  if AProvider = nil then
    Result := TdxWebServiceCalendarList.Create
  else
    Result := AProvider.GetCalendarList;
end;

function TcxSchedulerCloudStorageProviderCalendarIDProperty.GetProvider: TcxSchedulerWebServiceStorageCustomProvider;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to PropCount - 1 do
    if GetComponent(I) is TcxSchedulerWebServiceStorageCustomProvider then
      Exit(TcxSchedulerWebServiceStorageCustomProvider(GetComponent(I)));
end;

procedure Register;
begin
  RegisterComponents(dxCoreLibraryProductPage, [TcxSchedulerWebServiceStorage]);
  RegisterSelectionEditor(TcxSchedulerWebServiceStorage, TcxSchedulerWebServiceStorageSelectionEditor);
  RegisterPropertyEditor(TypeInfo(TcxSchedulerWebServiceStorageCustomProvider), TcxSchedulerWebServiceStorageResourceItem, 'Provider', TcxSchedulerWebServiceStorageProviderProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxSchedulerWebServiceStorageCustomProvider, 'CalendarID', TcxSchedulerCloudStorageProviderCalendarIDProperty);
  HideClassProperties(TcxSchedulerWebServiceStorage, ['StoreUsingGlobalTime']);
  HideClassProperties(TcxSchedulerWebServiceStorageResourceItem, ['ReadOnly', 'ProviderClassName', 'Parent', 'ResourceID', 'WorkDays', 'WorkFinish', 'WorkStart']);
end;

end.
