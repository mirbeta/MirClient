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

unit cxSchedulerReg;

{$I cxVer.inc}

interface

uses
  Classes, SysUtils, Menus,
  Types, DesignIntf, DesignEditors,  DesignMenus, VCLEditors,
  cxDesignWindows, cxEditPropEditors, cxPropEditors, cxControls, DB, cxDataStorage,
  cxDateUtils, cxSchedulerStorage, cxSchedulerDBStorage, cxSchedulerUtils,
  cxSchedulerStrs, cxSchedulerDialogs, cxScheduler, cxStyles, cxSchedulerDateNavigator,
  cxSchedulerCustomResourceView, cxSchedulerWeekView, cxSchedulerYearView,
  cxSchedulerTimeGridView, cxDateNavigator, ImgList, Controls, cxSchedulerHolidays,
  cxSchedulerAggregateStorage, dxCore, cxExportSchedulerLink, cxSchedulerAgendaView;

type
  TcxSchedulerSelectionEditor = class(TSelectionEditor)
  protected
    ComponentsList: TStringList;
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure Register;

function cxSchedulerComponentEditor: TComponentEditorClass;

implementation

uses
  RTLConsts, Windows, ShellApi, dxCoreClasses, cxSchedulerCustomControls, cxSchedulerDayView,
  cxSchedulerGanttView, cxClasses, dxCoreReg, cxLibraryReg, cxGraphics, cxSchedulerEditorFormManager,
  dxUIGeneratorDesignHelpers, dxBuiltInPopupMenu;

const
  scxSchedulerDescription = 'ExpressScheduler';
  scxSchedulerActiveView  = 'Active view';
  scxStorageDescription   = 'ExpressSchedulerStorage';
  scxEventsEditor         = 'Events editor...';
  scxDefault              = 'System';
  scxHolidaysEditor       = 'Edit...';

  ViewNames: array[0..6] of string =
    ('Day', 'Week', 'Weeks', 'Year', 'TimeGrid', 'Gantt', 'Agenda');
  ViewClasses: array[0..6] of TClass =
    (TcxSchedulerDayView, TcxSchedulerWeekView, TcxSchedulerWeeksView,
    TcxSchedulerYearView, TcxSchedulerTimeGridView, TcxSchedulerGanttView,
    TcxSchedulerAgendaView);

type

  { TcxDialogsStyleProperty }

  TcxDialogsStyleProperty = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TcxSchedulerComponentsEditor }

  TcxSchedulerComponentsEditor = class(TdxUIGeneratorComponentEditor)
  protected
    procedure ActiveViewClick(Sender: TObject);
    function CanSelectActiveView: Boolean;
    function GetViewByIndex(AIndex: Integer): TcxSchedulerCustomResourceView;

    function GetProductName: string; override;
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
  public
    procedure PrepareItem(Index: Integer;
      const AItem: TDesignMenuItem); override;
  end;

  { TcxSchedulerHolidayComponentsEditor }

  TcxSchedulerHolidayComponentsEditor = class(TcxSchedulerComponentsEditor)
  protected
    procedure InternalExecuteVerb(AIndex: Integer); override;
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
  end;

  { TcxSchedulerDBStorageFieldProperty }

  TcxSchedulerDBStorageFieldProperty = class(TFieldNameProperty)
  public
    function GetDataSource: TDataSource; override;
  end;

  { TcxSchedulerDBStorageFieldNameProperty }

  TcxSchedulerDBStorageFieldNameProperty = class(TFieldNameProperty)
  public
    function GetDataSource: TDataSource; override;
  end;

  { TcxSchedulerDBStorageResourceFieldNameProperty }

  TcxSchedulerDBStorageResourceFieldNameProperty = class(TFieldNameProperty)
  public
    function GetDataSource: TDataSource; override;
  end;

  { TcxSchedulerStylesEventsProperty }

  TcxSchedulerStylesEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TcxSchedulerContentPopupMenuEventsProperty }

  TcxSchedulerContentPopupMenuEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TcxSchedulerEventPopupMenuEventsProperty }

  TcxSchedulerEventPopupMenuEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TcxSchedulerStorageResourcesEventsProperty }

  TcxSchedulerStorageResourcesEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TcxSchedulerStorageRemindersEventsProperty }

  TcxSchedulerStorageRemindersEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TcxSchedulerNavigatorEventsProperty }

  TcxSchedulerNavigatorEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TcxSchedulerDateNavigatorEventsProperty }

  TcxSchedulerDateNavigatorEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TcxSchedulerViewDayEventsProperty }

  TcxSchedulerViewDayEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TcxSchedulerTimeZoneProperty }

  TcxSchedulerTimeZoneProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TcxSchedulerTimeScaleProperty }

  TcxSchedulerTimeScaleProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TcxSchedulerViewYearEventsProperty }

  TcxSchedulerViewYearEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TcxSchedulerViewYearScaleProperty }

  TcxSchedulerViewYearScaleProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TcxSchedulerViewTimeGridEventsProperty }

  TcxSchedulerViewTimeGridEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TcxSchedulerViewGanttEventsProperty }

  TcxSchedulerViewGanttEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;


  { TcxSchedulerViewTimeGridStylesEventsProperty }

  TcxSchedulerViewTimeGridStylesEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TcxSchedulerStorageResourceImageIndexProperty }

  TcxSchedulerStorageResourceImageIndexProperty = class(TImageIndexProperty)
  public
    function GetImages: TCustomImageList; override;
  end;

  { TcxSchedulerHolidayPropertyEditor }

  TcxSchedulerHolidayPropertyEditor = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TcxDateNavigatorSelectionEditor }

  TcxDateNavigatorSelectionEditor = class(TSelectionEditor)
  protected
    ComponentsList: TStringList;
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

function cxShowSchedulerHolidaysComponentEditor(AHolidays: TcxSchedulerHolidays): Boolean;
begin
  Result := cxShowHolidaysEditor(AHolidays, nil);
end;

{ TcxDialogsStyleProperty }

function TcxDialogsStyleProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueList, paSortList] - [paReadOnly];
end;

function TcxDialogsStyleProperty.GetValue: string;
begin
  Result := inherited GetValue;
  if Result = '' then
    Result := Format('Default (%s)', [cxSchedulerEditorManager.CurrentEditorFormStyle]);
end;

procedure TcxDialogsStyleProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to cxSchedulerEditorManager.Count - 1 do
    Proc(cxSchedulerEditorManager.Items[I].GetName);
end;

{ TcxSchedulerComponentsEditor }

procedure TcxSchedulerComponentsEditor.PrepareItem(Index: Integer;
  const AItem: TDesignMenuItem);
var
  I: Integer;

  function GetItemChecked(AIndex: Integer): Boolean;
  begin
    Result := (TcxScheduler(Component).CurrentView <> nil) and
      (TcxScheduler(Component).CurrentView.InheritsFrom(ViewClasses[AIndex]));
  end;

begin
  inherited;
  if (Index <> 0) or not CanSelectActiveView then Exit;
  for I := 0 to High(ViewNames) do
    AItem.AddItem(ViewNames[I], 0, GetViewByIndex(I).Active,
      GetViewByIndex(I).CanShow, ActiveViewClick);
end;

procedure TcxSchedulerComponentsEditor.ActiveViewClick(Sender: TObject);
begin
  GetViewByIndex((Sender as TMenuItem).MenuIndex).Active := True;
end;

function TcxSchedulerComponentsEditor.CanSelectActiveView: Boolean;
begin
  Result := Component is TcxScheduler;
end;

function TcxSchedulerComponentsEditor.GetViewByIndex(AIndex: Integer): TcxSchedulerCustomResourceView;
begin
  case AIndex of
   0:
     Result := TcxScheduler(Component).ViewDay;
   1:
     Result := TcxScheduler(Component).ViewWeek;
   2:
     Result := TcxScheduler(Component).ViewWeeks;
   3:
     Result := TcxScheduler(Component).ViewYear;
   4:
     Result := TcxScheduler(Component).ViewTimeGrid;
   5:
     Result := TcxScheduler(Component).ViewGantt;
   6:
     Result := TcxScheduler(Component).ViewAgenda;
   else
     Result := nil;
  end;
end;

function TcxSchedulerComponentsEditor.GetProductName: string;
begin
  Result := scxSchedulerDescription;
end;

function TcxSchedulerComponentsEditor.InternalGetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := scxSchedulerActiveView;
  else
    Result := inherited InternalGetVerb(AIndex);
  end;
end;

function TcxSchedulerComponentsEditor.InternalGetVerbCount: Integer;
begin
  if CanSelectActiveView then
    Result := 1
  else
    Result := 0;
end;

{ TcxSchedulerHolidayComponentsEditor }

procedure TcxSchedulerHolidayComponentsEditor.InternalExecuteVerb(AIndex: Integer);
begin
  if (AIndex = 0) and
      cxShowSchedulerHolidaysComponentEditor((GetComponent as TcxSchedulerHolidays)) then
    Designer.Modified
  else
    inherited InternalExecuteVerb(AIndex);
end;

function TcxSchedulerHolidayComponentsEditor.InternalGetVerb(AIndex: Integer): string;
begin
  Result := scxHolidaysEditor;
end;

function TcxSchedulerHolidayComponentsEditor.InternalGetVerbCount: Integer;
begin
  Result := 1;
end;

{ TcxSchedulerDBStorageFieldProperty }

function TcxSchedulerDBStorageFieldProperty.GetDataSource: TDataSource;
begin
  Result := TcxSchedulerDBStorageField(GetComponent(0)).Storage.DataSource;
end;

{ TcxSchedulerDBStorageFieldNameProperty }

function TcxSchedulerDBStorageFieldNameProperty.GetDataSource: TDataSource;
begin
  Result := TcxSchedulerDBStorageFieldNames(GetComponent(0)).Storage.DataSource;
end;

{ TcxSchedulerDBStorageResourceFieldNameProperty }

function TcxSchedulerDBStorageResourceFieldNameProperty.GetDataSource: TDataSource;
begin
  Result := TcxSchedulerDBStorageResources(GetComponent(0)).DataSource;
end;

{ TcxSchedulerTimeZoneProperty }

function TcxSchedulerTimeZoneProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  Result := Result - [paReadOnly, paSubProperties] + [paValueList, paRevertable];
end;

function TcxSchedulerTimeZoneProperty.GetValue: string;
var
  AIndex: Integer;
begin
  AIndex := GetOrdValue;
  if (AIndex < 0) or (AIndex >= DateTimeHelper.TimeZoneCount) then
    Result := scxDefault
  else
    Result := DateTimeHelper.TimeZoneInfo(AIndex).Display;
end;

procedure TcxSchedulerTimeZoneProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  AList: TStringList;
begin
  Proc(scxDefault);
  AList := TStringList.Create;
  try
    for I := 0 to DateTimeHelper.TimeZoneCount - 1 do
      AList.Add(DateTimeHelper.TimeZoneInfo(I).Display);
    AList.Sort;
    for I := 0 to AList.Count - 1 do
      Proc(AList[I]);
  finally
    AList.Free;
  end;
end;

procedure TcxSchedulerTimeZoneProperty.SetValue(const Value: string);
var
  AIndex: Integer;
begin
  if (Value = '') or (Value = scxDefault) then
    AIndex := -1
  else
  begin
    AIndex := DateTimeHelper.TimeZoneCount - 1;
    while AIndex >= 0 do
    begin
      if AnsiCompareText(DateTimeHelper.TimeZoneInfo(AIndex).Display, Value) = 0 then
        Break
      else
        Dec(AIndex);
    end;
  end;
  SetOrdValue(AIndex);
end;

{ TcxSchedulerTimeScaleProperty }

function TcxSchedulerTimeScaleProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  Result := Result - [paReadOnly, paSubProperties] + [paValueList, paRevertable];
end;

function TcxSchedulerTimeScaleProperty.GetValue: string;
begin
  Result := IntToStr(GetOrdValue) + ' ' + scxMinutes;
end;

procedure TcxSchedulerTimeScaleProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  AHasLastValue: Boolean;
const
  AValues: array[0..6] of Integer = (60, 30, 15, 10, 6, 5, 1);
begin
  AHasLastValue := GetComponent(0) is TcxSchedulerTimeGridViewScales;
  for I := 0 to High(AValues) - 1 + Byte(AHasLastValue) do
    Proc(IntToStr(AValues[I]) + ' ' + scxMinutes);
end;

procedure TcxSchedulerTimeScaleProperty.SetValue(const Value: string);
var
  I: Integer;
  S: string;
  ATime, ACode: Integer;
begin
  S := '';
  for I := 1 to Length(Value) do
    if dxCharInSet(Value[I], ['0'..'9']) then
       S := S + Value[I]
    else
      Break;
  if S = '' then S := '60';
  Val(S, ATime, ACode);
  if ACode <> 0 then
    ATime := 60;
  SetOrdValue(ATime);
end;

{ TcxSchedulerStylesEventsProperty }

function TcxSchedulerStylesEventsProperty.GetInstance: TPersistent;
begin
  if (GetComponent(0) is TcxCustomScheduler) then
    Result := TcxCustomScheduler(GetComponent(0)).Styles
  else
    Result := nil;
end;

{ TcxSchedulerContentPopupMenuEventsProperty }

function TcxSchedulerContentPopupMenuEventsProperty.GetInstance: TPersistent;
begin
  if (GetComponent(0) is TcxCustomScheduler) then
    Result := TcxCustomScheduler(GetComponent(0)).ContentPopupMenu
  else
    Result := nil;
end;

{ TcxSchedulerEventPopupMenuEventsProperty }

function TcxSchedulerEventPopupMenuEventsProperty.GetInstance: TPersistent;
begin
  if (GetComponent(0) is TcxCustomScheduler) then
    Result := TcxCustomScheduler(GetComponent(0)).EventPopupMenu
  else
    Result := nil;
end;

{ TcxSchedulerStorageResourcesEventsProperty }

function TcxSchedulerStorageResourcesEventsProperty.GetInstance: TPersistent;
begin
  if (GetComponent(0) is TcxCustomSchedulerStorage) then
    Result := TcxCustomSchedulerStorage(GetComponent(0)).Resources
  else
    Result := nil;
end;

{ TcxSchedulerStorageRemindersEventsProperty }

function TcxSchedulerStorageRemindersEventsProperty.GetInstance: TPersistent;
begin
  if (GetComponent(0) is TcxCustomSchedulerStorage) then
    Result := TcxCustomSchedulerStorage(GetComponent(0)).Reminders
  else
    Result := nil;
end;

{ TcxSchedulerNavigatorEventsProperty }

function TcxSchedulerNavigatorEventsProperty.GetInstance: TPersistent;
begin
  if (GetComponent(0) is TcxCustomScheduler) then
    Result := TcxCustomScheduler(GetComponent(0)).ResourceNavigator
  else
    Result := nil;
end;

{ TcxSchedulerDateNavigatorEventsProperty }

function TcxSchedulerDateNavigatorEventsProperty.GetInstance: TPersistent;
begin
  if (GetComponent(0) is TcxScheduler) then
    Result := TcxScheduler(GetComponent(0)).DateNavigator
  else
    Result := nil;
end;

{ TcxSchedulerViewDayEventsProperty }

function TcxSchedulerViewDayEventsProperty.GetInstance: TPersistent;
begin
  if (GetComponent(0) is TcxScheduler) then
    Result := TcxScheduler(GetComponent(0)).ViewDay
  else
    Result := nil;
end;

{ TcxSchedulerViewYearEventsProperty }

function TcxSchedulerViewYearEventsProperty.GetInstance: TPersistent;
begin
  if (GetComponent(0) is TcxScheduler) then
    Result := TcxScheduler(GetComponent(0)).ViewYear
  else
    Result := nil;
end;

{ TcxSchedulerViewYearScaleProperty }

function TcxSchedulerViewYearScaleProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  Result := Result - [paReadOnly, paSubProperties] + [paValueList, paRevertable];
end;

function TcxSchedulerViewYearScaleProperty.GetValue: string;
begin
  case GetOrdValue of
    12: Result := scxFullYear;
    6:  Result := scxHalfYear;
    3:  Result := scxQuarter;
  end;
end;

procedure TcxSchedulerViewYearScaleProperty.GetValues(Proc: TGetStrProc);
begin
  Proc(scxFullYear);
  Proc(scxHalfYear);
  Proc(scxQuarter);
end;

procedure TcxSchedulerViewYearScaleProperty.SetValue(const Value: string);
begin
  if Value = scxFullYear then
    SetOrdValue(12)
  else
    if Value = scxHalfYear then
      SetOrdValue(6)
    else
      SetOrdValue(3);
end;

{ TcxSchedulerViewTimeGridEventsProperty }

function TcxSchedulerViewTimeGridEventsProperty.GetInstance: TPersistent;
begin
  if (GetComponent(0) is TcxScheduler) then
    Result := TcxScheduler(GetComponent(0)).ViewTimeGrid
  else
    Result := nil;
end;

{ TcxSchedulerViewGanttEventsProperty }

function TcxSchedulerViewGanttEventsProperty.GetInstance: TPersistent;
begin
  if (GetComponent(0) is TcxScheduler) then
    Result := TcxScheduler(GetComponent(0)).ViewGantt
  else
    Result := nil;
end;

{  TcxSchedulerViewTimeGridStylesEventsProperty }

function TcxSchedulerViewTimeGridStylesEventsProperty.GetInstance: TPersistent;
begin
  if (GetComponent(0) is TcxSchedulerTimeGridView) then
    Result := TcxSchedulerTimeGridView(GetComponent(0)).Styles
  else
    Result := nil;
end;

{ TcxSchedulerStorageResourceImageIndexProperty }

function TcxSchedulerStorageResourceImageIndexProperty.GetImages: TCustomImageList;
begin
  if (GetComponent(0) is TcxSchedulerStorageResourceItem) then
    Result := TcxSchedulerStorageResourceItem(GetComponent(0)).Resources.Images
  else
    Result := nil;
end;

{ TcxSchedulerHolidayPropertyEditor }

procedure TcxSchedulerHolidayPropertyEditor.Edit;
begin
  if cxShowSchedulerHolidaysComponentEditor((GetComponent(0) as TcxSchedulerHolidays)) then
    Designer.Modified;
end;

function TcxSchedulerHolidayPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{ TcxSchedulerSelectionEditor }

procedure TcxSchedulerSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
{$IFDEF DELPHI16}
  Proc('Vcl.Menus');
{$ELSE}
  Proc('Menus');
{$ENDIF}
  Proc('cxStyles');
  Proc('cxGraphics');
  Proc('cxEdit');
  Proc('cxScheduler');
  Proc('cxSchedulerStorage');
  Proc('cxSchedulerCustomControls');
  Proc('cxSchedulerCustomResourceView');
  Proc('cxSchedulerDayView');
  Proc('cxSchedulerAgendaView');
  Proc('cxSchedulerDateNavigator');
  Proc('cxSchedulerHolidays');
  Proc('cxSchedulerTimeGridView');
  Proc('cxSchedulerUtils');
  Proc('cxSchedulerWeekView');
  Proc('cxSchedulerYearView');
  Proc('cxSchedulerGanttView');
  Proc('cxSchedulerRecurrence');
  if not TdxBuiltInPopupMenuAdapterManager.IsActualAdapterStandard then
    Proc(cxGetUnitName(TdxBuiltInPopupMenuAdapterManager.GetActualAdapterClass));
end;

{ TcxDateNavigatorSelectionEditor }

procedure TcxDateNavigatorSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('cxStyles');
  Proc('cxGraphics');
  Proc('cxSchedulerStorage');
  Proc('cxSchedulerCustomControls');
  Proc('cxSchedulerDateNavigator');
end;

function cxSchedulerComponentEditor: TComponentEditorClass;
begin
  Result := TcxSchedulerComponentsEditor;
end;

procedure Register;
begin
  RegisterComponents(dxCoreLibraryProductPage, [TcxSchedulerAggregateStorage, TcxSchedulerStorage, TcxSchedulerDBStorage,
    TcxScheduler, TcxDateNavigator, TcxSchedulerHolidays]);
  RegisterClasses([TcxSchedulerStorageField, TcxSchedulerDBStorageField]);
  RegisterComponentEditor(TcxCustomScheduler, TcxSchedulerComponentsEditor);
  RegisterComponentEditor(TcxCustomSchedulerStorage, TcxSchedulerComponentsEditor);
  RegisterComponentEditor(TcxCustomDateNavigator, TcxSchedulerComponentsEditor);
  RegisterComponentEditor(TcxSchedulerHolidays, TcxSchedulerHolidayComponentsEditor);

  RegisterPropertyEditor(TypeInfo(string), TcxSchedulerStorageField,
    'ValueType', TcxValueTypeProperty);

  RegisterPropertyEditor(TypeInfo(string), TcxScheduler, 'DialogsStyle', TcxDialogsStyleProperty);


  // data base field names property editors
  RegisterPropertyEditor(TypeInfo(string), TcxSchedulerDBStorageField,
    'FieldName', TcxSchedulerDBStorageFieldProperty);
  RegisterPropertyEditor(TypeInfo(TcxFieldName), TcxSchedulerDBStorageFieldNames,
    '', TcxSchedulerDBStorageFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(TcxFieldName), TcxSchedulerDBStorageResources,
    '', TcxSchedulerDBStorageResourceFieldNameProperty);

  RegisterPropertyEditor(TypeInfo(TcxSchedulerHolidaysLocations), TcxSchedulerHolidays,
    'Locations', TcxSchedulerHolidayPropertyEditor);

  RegisterPropertyEditor(TypeInfo(Boolean), TcxSchedulerOptionsView,
    'RotateResourceCaptions', nil);

  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomScheduler,
    'StylesEvents', TcxSchedulerStylesEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxScheduler,
    'DateNavigatorEvents', TcxSchedulerDateNavigatorEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxScheduler,
    'ViewDayEvents', TcxSchedulerViewDayEventsProperty);

  RegisterPropertyEditor(TypeInfo(TcxImageIndex), TcxSchedulerStorageResourceItem,
    'ImageIndex', TcxSchedulerStorageResourceImageIndexProperty);

  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomSchedulerStorage,
    'ResourcesEvents', TcxSchedulerStorageResourcesEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomSchedulerStorage,
    'RemindersEvents', TcxSchedulerStorageRemindersEventsProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TcxSchedulerReminders,
    'TimeZone', TcxSchedulerTimeZoneProperty);

  // day view property editors
  RegisterPropertyEditor(TypeInfo(Integer), TcxSchedulerOptionsView,
    'CurrentTimeZone', TcxSchedulerTimeZoneProperty);
  RegisterPropertyEditor(TypeInfo(Integer),  TcxSchedulerOptionsView,
    'AdditionalTimeZone', TcxSchedulerTimeZoneProperty);
  RegisterPropertyEditor(TypeInfo(Integer),  TcxSchedulerDayView,
    'TimeScale', TcxSchedulerTimeScaleProperty);

  // TimeGrid view
  RegisterPropertyEditor(TypeInfo(Integer), TcxSchedulerTimeGridViewScales,
    'TimeStep', TcxSchedulerTimeScaleProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxScheduler,
    'ViewTimeGridEvents', TcxSchedulerViewTimeGridEventsProperty);

  // year view
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxScheduler,
    'ViewYearEvents', TcxSchedulerViewYearEventsProperty);
  RegisterPropertyEditor(TypeInfo(Integer),  TcxSchedulerYearView,
    'Scale', TcxSchedulerViewYearScaleProperty);

  // gantt view
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxScheduler,
    'ViewGanttEvents', TcxSchedulerViewGanttEventsProperty);

  //popups
  RegisterPropertyEditor(TypeInfo(TComponent), TcxSchedulerPopupMenu,
    'PopupMenu', TcxControlPopupMenuProperty);

  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomScheduler,
    'ResourceNavigatorEvents', TcxSchedulerNavigatorEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomScheduler,
    'ContentPopupMenuEvents', TcxSchedulerContentPopupMenuEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TcxCustomScheduler,
    'EventPopupMenuEvents', TcxSchedulerEventPopupMenuEventsProperty);

  RegisterSelectionEditor(TcxCustomScheduler, TcxSchedulerSelectionEditor);
  RegisterSelectionEditor(TcxDateNavigator, TcxDateNavigatorSelectionEditor);
end;

end.
