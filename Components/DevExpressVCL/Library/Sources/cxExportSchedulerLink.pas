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

unit cxExportSchedulerLink;

{$I cxVer.inc}

interface

uses
  Types, Windows, Classes, SysUtils, Graphics, Variants, cxDataUtils, cxClasses, cxGraphics, cxStyles,
  cxGeometry, cxSchedulerCustomControls, Math, cxSchedulerUtils, cxSchedulerStorage, cxSchedulerStrs, cxExport,
  Forms, Menus, cxLookAndFeelPainters, Controls, cxControls, cxContainer, cxEdit, cxTextEdit, cxMaskEdit, cxCalendar,
  cxDropDownEdit, StdCtrls, cxButtons, cxLookAndFeels, cxDateUtils, cxLabel, ComCtrls, dxCore, dxLayoutContainer,
  dxLayoutControl, dxLayoutLookAndFeels, dxLayoutcxEditAdapters, dxLayoutControlAdapters, dxForms;

procedure cxExportSchedulerToHTML(const AFileName: string; AScheduler: TcxCustomScheduler; AsTable: Boolean = False;
  AShowDialog: Boolean = False;  const AHeader: string = 'Event %d'; const AStart: TDateTime = NullDate;
  const AFinish: TDateTime = NullDate; const AFileExt: string = 'html'; AHandler: TObject = nil);
procedure cxExportSchedulerToXML(const AFileName: string; AScheduler: TcxCustomScheduler; AsTable: Boolean = False;
  AShowDialog: Boolean = False; const AHeader: string = 'Event %d'; const AStart: TDateTime = NullDate;
  const AFinish: TDateTime = NullDate; const AFileExt: string = 'xml'; AHandler: TObject = nil);

procedure cxExportSchedulerToExcel(const AFileName: string; AScheduler: TcxCustomScheduler; AsTable: Boolean = False;
  AShowDialog: Boolean = False; const AHeader: string = 'Event %d'; const AStart: TDateTime = NullDate;
  const AFinish: TDateTime = NullDate; const AFileExt: string = 'xls'; AHandler: TObject = nil);
procedure cxExportSchedulerToXLSX(const AFileName: string; AScheduler: TcxCustomScheduler; AsTable: Boolean = False;
  AShowDialog: Boolean = False; const AHeader: string = 'Event %d'; const AStart: TDateTime = NullDate;
  const AFinish: TDateTime = NullDate; const AFileExt: string = 'xlsx'; AHandler: TObject = nil);

procedure cxExportSchedulerToCSV(const AFileName: string; AScheduler: TcxCustomScheduler; AsTable: Boolean = False;
  AShowDialog: Boolean = False; const AHeader: string = 'Event %d'; const AStart: TDateTime = NullDate;
  const AFinish: TDateTime = NullDate; const ASeparator: Char = ','; const AFileExt: string = 'csv';
  AHandler: TObject = nil; AEncoding: TEncoding = nil);

procedure cxExportSchedulerToText(const AFileName: string; AScheduler: TcxCustomScheduler; AsTable: Boolean;
  AShowDialog: Boolean = False; const AStart: TDateTime = NullDate; const AFinish: TDateTime = NullDate;
  const AFileExt: string = 'txt'; AHandler: TObject = nil; AEncoding: TEncoding = nil); overload;
procedure cxExportSchedulerToText(const AFileName: string; AScheduler: TcxCustomScheduler; AsTable: Boolean = False;
  AShowDialog: Boolean = False; const AHeader: string = 'Event %d'; const AStart: TDateTime = NullDate;
  const AFinish: TDateTime = NullDate; const ASeparator: string = ''; const ABeginString: string = '';
  const AEndString: string = ''; const AFileExt: string = 'txt'; AHandler: TObject = nil;
  AEncoding: TEncoding = nil); overload;

//  general export method interface
procedure cxExportSchedulerToFile(AFileName: string; AScheduler: TcxCustomScheduler; AsTable, AShowDialog: Boolean;
  const AHeader: string; AStart, AFinish: TDateTime; AExportType: Integer; const ASeparators: array of string;
  const AFileExt: string; AHandler: TObject = nil; AEncoding: TEncoding = nil);

type

  {TcxSchedulerExportHelper }

  TcxSchedulerExportHelper = class
  private
    FEvents: TcxSchedulerFilteredEventList;
    FFinish: TDateTime;
    FHandler: TObject;
    FHeader: string;
    FNameProvider: IcxNameExportProvider;
    FProgressHelper: TcxProgressCalculationHelper;
    FProvider: IcxExportProvider;
    FStart: TDateTime;

    procedure ProgressHandler(Sender: TObject; Percents: Integer);
  protected
    HasHeader: Boolean;
    CaptionStyle: Integer;
    Font: TFont;

    procedure CalculateLayout; virtual;
    procedure DoExport; virtual;
    procedure ExportEvent(AEvent: TcxSchedulerEvent; AIndex: Integer; var ARow: Integer); virtual;
    procedure ExportEvents; virtual;
    function RegisterCellStyle(AColor, AFontColor: TColor; AFontStyle: TFontStyles;
      ABorders, ABoldBorders: TcxBorders; AAlignText: TcxAlignText = catLeft): Integer;
  public
    constructor Create(AScheduler: TcxCustomScheduler; const AStart, AFinish: TDateTime;
      AExportType: Integer; const AFileName: string; AHandler: TObject = nil); virtual;
    destructor Destroy; override;
    //
    property Events: TcxSchedulerFilteredEventList read FEvents;
    property Finish: TDateTime read FFinish;
    property Header: string read FHeader;
    property NameProvider: IcxNameExportProvider read FNameProvider;
    property ProgressHelper: TcxProgressCalculationHelper read FProgressHelper;
    property Provider: IcxExportProvider read FProvider;
    property Start: TDateTime read FStart;
  end;

  { TcxSchedulerTableExportHelper }

  TcxSchedulerTableExportHelper = class(TcxSchedulerExportHelper)
  protected
    Style: Integer;
    procedure CalculateLayout; override;
    procedure DoExport; override;
    procedure ExportEvent(AEvent: TcxSchedulerEvent; AIndex: Integer;  var ARow: Integer); override;
  end;

  TcxSchedulerExportHelperClass = class of TcxSchedulerExportHelper;


  { TfmExportRangeDialog }

  TfmExportRangeDialog = class(TdxForm)
    btnOk: TcxButton;
    btnCancel: TcxButton;
    lcMain: TdxLayoutControl;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    lbSetDateRange: TdxLayoutLabeledItem;
    dxLayoutItem1: TdxLayoutItem;
    deStart: TcxDateEdit;
    dxLayoutItem2: TdxLayoutItem;
    deFinish: TcxDateEdit;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    lbAnd: TdxLayoutLabeledItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    procedure deDatePropertiesChange(Sender: TObject);
  protected
    procedure DoShow; override;
    function GetFormColor: TColor; virtual;
    function IsValid: Boolean;
    procedure SetCaptions; virtual;
  public
    constructor CreateEx(ALookAndFeel: TcxLookAndFeel; AStart, AFinish: TDateTime); virtual;
  end;

const
  SchedulerExportHelpers: array[Boolean] of TcxSchedulerExportHelperClass =
    (TcxSchedulerExportHelper, TcxSchedulerTableExportHelper);

  YesNoStrs: array[Boolean] of Pointer = (@secxNo, @secxYes);
  TrueFalseStrs: array[Boolean] of Pointer = (@secxFalse, @secxTrue);
  States: array[0..3] of Pointer = (@scxFree, @scxTentative, @scxBusy, @scxOutOfOffice);
  MaxColumnWidth: Integer = 20;

implementation

uses
  cxSchedulerDialogs, DateUtils;

{$R *.dfm}

const
  NecessaryFields: array[0..12] of Pointer =
     (@secxSubject, @secxStartDate, @secxStartTime, @secxEndDate, @secxEndTime,
      @secxAlldayevent, @secxReminderonoff, @secxReminderDate, @secxReminderTime,
      @secxCategories, @secxDescription, @secxLocation, @secxShowtimeas);

type
  TcxDateNavigatorAccess = class(TcxSchedulerCustomDateNavigator);

function BoolToStr(AValue: Boolean): string;
begin
  Result := cxGetResourceString(TrueFalseStrs[AValue]);
end;

function YesNoToStr(AValue: Boolean): string;
begin
  Result := cxGetResourceString(YesNoStrs[AValue]);
end;

function StateToStr(AState: Integer): string;
begin
  case AState of
    0..3:
      Result := cxGetResourceString(States[AState]);
  else
    Result := '';
  end;
end;

function LabelColorToStr(AColor: Integer): string;
var
  I: Integer;
begin
  Result := '';
  I := EventLabels.IndexOfColor(AColor);
  if I >= 0 then
  begin
    Result := EventLabels[I].Caption;
    if SameText(cxGetResourceString(@scxEventLabelNone), Result) then
      Result := '';
  end;
end;

function ShowSetRangeDialog(ALookAndFeel: TcxLookAndFeel; var AStart: TDateTime; var AFinish: TDateTime): Boolean;
begin
  if AStart = NullDate then
  begin
    AStart := dxGetStartDateOfMonth(Date);
    AFinish := IncMonth(AStart, 4);
  end;
  with TfmExportRangeDialog.CreateEx(ALookAndFeel, AStart, AFinish) do
  try
    Result := ShowModal = mrOk;
    if Result then
    begin
      AStart := deStart.Date;
      AFinish := deFinish.Date - MinuteToTime;
    end;
  finally
    Release;
  end;
end;

{ TcxSchedulerExportHelper }

constructor TcxSchedulerExportHelper.Create(AScheduler: TcxCustomScheduler; const AStart, AFinish: TDateTime;
  AExportType: Integer; const AFileName: string; AHandler: TObject = nil);
begin
  FHandler := AHandler;
  FEvents := TcxSchedulerFilteredEventList.Create;
  TcxExport.Provider(AExportType, AFileName).GetInterface(IcxExportProvider, FProvider);
  FProgressHelper := TcxProgressCalculationHelper.Create(2, AScheduler, ProgressHandler);
  Supports(Provider, IcxNameExportProvider, FNameProvider);
  FStart := AStart;
  FFinish := AFinish;
  AScheduler.Storage.GetEvents(FEvents, AStart, AFinish);
  Provider.SetDefaultStyle(DefaultCellStyle);
  Font := TFont.Create;
  Font.Name := DefaultCellStyle.FontName;
  Font.Size := DefaultCellStyle.FontSize;
end;

destructor TcxSchedulerExportHelper.Destroy;
begin
  FEvents.Free;
  try
    Font.Free;
    FreeAndNil(FProgressHelper);
    FProvider := nil;
  finally
    inherited Destroy;
  end;
end;

procedure TcxSchedulerExportHelper.CalculateLayout;
begin
  if Events.Count = 0 then Exit;
  HasHeader := FHeader <> '';
  Provider.SetRange(2, Events.Count * (11 + Byte(HasHeader)), False);
end;

procedure TcxSchedulerExportHelper.DoExport;
begin
  CalculateLayout;
  ExportEvents;
  Provider.Commit(ProgressHelper, FHandler);
end;

procedure TcxSchedulerExportHelper.ExportEvent(
  AEvent: TcxSchedulerEvent; AIndex: Integer; var ARow: Integer);

  procedure WriteSingleRow(const ACaption: string; const AData: Variant; ABorders: TcxBorders);
  begin
    Provider.SetCellStyle(0, ARow, RegisterCellStyle(clDefault,
      clDefault, [fsBold], cxBordersAll, ABorders));
    Provider.SetCellStyle(1, ARow, RegisterCellStyle(clDefault,
      clDefault, [], cxBordersAll, ABorders - [bLeft]));
    Provider.SetCellValue(0, ARow, ACaption);
    Provider.SetCellValue(1, ARow, AData);
    Inc(ARow);
  end;

const
  ATopRowBorders: array[Boolean] of TcxBorders =
    ([bLeft, bTop, bRight], [bLeft, bRight]);
begin
  if HasHeader then
  begin
    Provider.SetCellStyle(cxRectBounds(0, ARow, 1, 2),
      RegisterCellStyle(clDefault, clDefault, [fsBold], cxBordersAll, [bRight, bLeft, bTop], catCenter));
    Provider.SetCellValue(0, ARow, Format(Header, [AIndex]));
    Inc(ARow);
  end;
  WriteSingleRow(cxGetResourceString(@secxSubject),
    AEvent.Caption, ATopRowBorders[HasHeader]);
  WriteSingleRow(cxGetResourceString(@secxLocation),
    AEvent.Location, [bLeft, bRight]);
  WriteSingleRow(cxGetResourceString(@secxDescription),
    AEvent.Message, [bLeft, bRight]);
  WriteSingleRow(cxGetResourceString(@secxAllDay),
    YesNoToStr(AEvent.AllDayEvent), [bLeft, bRight]);
  WriteSingleRow(cxGetResourceString(@secxStart), AEvent.Start, [bLeft, bRight]);
  WriteSingleRow(cxGetResourceString(@secxFinish), AEvent.Finish, [bLeft, bRight]);
  WriteSingleRow(cxGetResourceString(@secxState),
    StateToStr(AEvent.State), [bLeft, bBottom, bRight]);
  WriteSingleRow(cxGetResourceString(@secxReminder),
    BoolToStr(AEvent.Reminder), [bLeft, bBottom, bRight]);
  WriteSingleRow(cxGetResourceString(@secxReminderDate), VarToDateTime(AEvent.ReminderDate), [bLeft, bRight]);
  WriteSingleRow(cxGetResourceString(@secxCategories), LabelColorToStr(AEvent.LabelColor), [bLeft, bRight]);
  Provider.SetCellStyle(0, ARow, RegisterCellStyle(clDefault, clDefault, [], [], []));
  Provider.SetCellStyle(1, ARow, RegisterCellStyle(clDefault, clDefault, [], [], []));
  Inc(ARow);
end;

procedure TcxSchedulerExportHelper.ExportEvents;
var
  I, ARow: Integer;
begin
  ARow := 0;
  ProgressHelper.BeginStage(Events.Count);
  try
    for I := 0 to Events.Count - 1 do
    begin
      ExportEvent(Events[I], I, ARow);
      ProgressHelper.NextTask;
    end;
  finally
    ProgressHelper.EndStage;
  end;
end;

function TcxSchedulerExportHelper.RegisterCellStyle(AColor, AFontColor: TColor;
  AFontStyle: TFontStyles; ABorders, ABoldBorders: TcxBorders; AAlignText: TcxAlignText = catLeft): Integer;
var
  AStyle: TcxCacheCellStyle;
  ASide: TcxBorder;
begin
  AStyle := DefaultCellStyle;
  AStyle.FontStyle := AFontStyle;
  AStyle.AlignText := AAlignText;
  if AColor <> clDefault then
  begin
    AStyle.BrushStyle := cbsSolid;
    AStyle.BrushBkColor := ColorToRgb(AColor);
  end;
  if AFontColor <> clDefault then
    AStyle.FontColor := AFontColor;
  for ASide := bLeft to bBottom do
    with AStyle.Borders[Integer(ASide)] do
    begin
      IsDefault := not (ASide in ABorders);
      if not IsDefault then
      begin
        if ASide in ABoldBorders then
          Width := 2
        else
          Width := 1;
      end;
    end;
  Result := Provider.RegisterStyle(AStyle);
end;

procedure TcxSchedulerExportHelper.ProgressHandler(Sender: TObject; Percents: Integer);
var
  AIntf: IcxExportProgress;
begin
  if Supports(FHandler, IcxExportProgress, AIntf) then
    AIntf.OnProgress(Sender, Percents);
end;

{ TcxSchedulerTableExportHelper }

procedure TcxSchedulerTableExportHelper.CalculateLayout;
begin
  HasHeader := FHeader <> '';
  Provider.SetRange(Length(NecessaryFields), Events.Count + 1 , False);
end;

procedure TcxSchedulerTableExportHelper.DoExport;
var
  I: Integer;
begin
  CalculateLayout;
  Style := RegisterCellStyle(clDefault, clDefault, [fsBold], cxBordersAll, [], catCenter);
  if FNameProvider <> nil then
  begin
    FNameProvider.SetName('Calendar');
    FNameProvider.SetRangeName('Calendar', Rect(0, 0, Length(NecessaryFields) - 1, Events.Count));
  end;
  for I := 0 to Length(NecessaryFields) - 1 do
  begin
    Provider.SetCellStyle(I, 0, Style);
    Provider.SetCellValue(I, 0, cxGetResourceString(NecessaryFields[I]));
  end;
  Style := RegisterCellStyle(clDefault, clDefault, [], cxBordersAll, []);
  ExportEvents;
  Provider.Commit(ProgressHelper, FHandler);
end;

procedure TcxSchedulerTableExportHelper.ExportEvent(
  AEvent: TcxSchedulerEvent; AIndex: Integer; var ARow: Integer);
var
  ACol: Integer;

  procedure WriteItem(const AData: Variant);
  begin
    Provider.SetCellStyle(ACol, ARow, Style);
    Provider.SetCellValue(ACol, ARow, AData);
    Inc(ACol);
  end;


const
  AStates: array[0..3] of Integer = (3, 1, 2, 4);
begin
  if ARow = 0 then // for fields header
    Inc(ARow);
  ACol := 0;
  WriteItem(AEvent.Caption);                      // Subject
  WriteItem(VarToDateTime(dxDateOf(AEvent.Start))); // StartDate
  WriteItem(VarToDateTime(dxTimeOf(AEvent.Start))); // StartTime
  WriteItem(VarToDateTime(dxDateOf(AEvent.Finish)));// EndDate
  WriteItem(VarToDateTime(dxTimeOf(AEvent.Finish)));// EndTime
  WriteItem(AEvent.AllDayEvent);                  // Alldayevent
  WriteItem(BoolToStr(AEvent.Reminder));          // Reminderonoff
  WriteItem(VarToDateTime(dxDateOf(AEvent.ReminderDate))); // ReminderDate
  WriteItem(VarToDateTime(dxTimeOf(AEvent.ReminderDate))); // ReminderTime
  WriteItem(LabelColorToStr(AEvent.LabelColor));  // Categories,
  WriteItem(AEvent.Message);                      // Description
  WriteItem(AEvent.Location);                     // Location
  WriteItem(AStates[Min(3, AEvent.State)]);       // Showtimeas
  Inc(ARow);
end;

// external procedures definition

procedure cxExportSchedulerToFile(AFileName: string; AScheduler: TcxCustomScheduler; AsTable, AShowDialog: Boolean;
  const AHeader: string; AStart, AFinish: TDateTime; AExportType: Integer; const ASeparators: array of string;
  const AFileExt: string; AHandler: TObject; AEncoding: TEncoding);
var
  AEncodingIntf: IcxExportProviderEncoding;
  AIntf: IcxExportWithSeparators;
  I: Integer;
begin
  if AFileExt <> '' then
    AFileName := ChangeFileExt(AFileName, '.' + AFileExt);
  if (AScheduler.Storage = nil) then
    cxSchedulerError(secxExportStorageInvalid);

  if not AShowDialog then
  begin
    if (AStart = NullDate) or (AFinish = NullDate) then
    begin
      AStart := TcxDateNavigatorAccess(AScheduler.DateNavigator).GetRealFirstDate;
      AFinish := TcxDateNavigatorAccess(AScheduler.DateNavigator).GetRealLastDate;
    end;
  end
  else
    if not ShowSetRangeDialog(AScheduler.DialogsLookAndFeel, AStart, AFinish) then
      Exit;

  with SchedulerExportHelpers[AsTable].Create(AScheduler, AStart, AFinish, AExportType, AFileName, AHandler) do
  try
    FHeader := AHeader;
    if Supports(Provider, IcxExportProviderEncoding, AEncodingIntf) then
      AEncodingIntf.SetEncoding(AEncoding);
    if Supports(Provider, IcxExportWithSeparators, AIntf) and (Length(ASeparators) > 0) then
    begin
      for I := Low(ASeparators) to High(ASeparators) do
        AIntf.AddSeparator(ASeparators[I]);
    end;
    DoExport;
  finally
    Free;
  end;
end;

procedure cxExportSchedulerToCSV(const AFileName: string; AScheduler: TcxCustomScheduler; AsTable: Boolean = False;
  AShowDialog: Boolean = False; const AHeader: string = 'Event %d'; const AStart: TDateTime = NullDate;
  const AFinish: TDateTime = NullDate; const ASeparator: Char = ','; const AFileExt: string = 'csv';
  AHandler: TObject = nil; AEncoding: TEncoding = nil);
begin
  cxExportSchedulerToFile(AFileName, AScheduler, AsTable, AShowDialog,
    AHeader, AStart, AFinish, cxExportToCSV, [ASeparator], AFileExt, AHandler, AEncoding);
end;

procedure cxExportSchedulerToHTML(const AFileName: string; AScheduler: TcxCustomScheduler; AsTable: Boolean = False;
  AShowDialog: Boolean = False; const AHeader: string = 'Event %d'; const AStart: TDateTime = NullDate;
  const AFinish: TDateTime = NullDate; const AFileExt: string = 'html'; AHandler: TObject = nil);
begin
  cxExportSchedulerToFile(AFileName, AScheduler, AsTable, AShowDialog,
    AHeader, AStart, AFinish, cxExportToHtml, [], AFileExt, AHandler);
end;

procedure cxExportSchedulerToXML(const AFileName: string; AScheduler: TcxCustomScheduler; AsTable: Boolean = False;
  AShowDialog: Boolean = False; const AHeader: string = 'Event %d'; const AStart: TDateTime = NullDate;
  const AFinish: TDateTime = NullDate; const AFileExt: string = 'xml'; AHandler: TObject = nil);
begin
  cxExportSchedulerToFile(AFileName, AScheduler, AsTable, AShowDialog,
    AHeader, AStart, AFinish, cxExportToXML, [], AFileExt, AHandler);
end;

procedure cxExportSchedulerToExcel(const AFileName: string; AScheduler: TcxCustomScheduler; AsTable: Boolean = False;
  AShowDialog: Boolean = False; const AHeader: string = 'Event %d'; const AStart: TDateTime = NullDate;
  const AFinish: TDateTime = NullDate; const AFileExt: string = 'xls'; AHandler: TObject = nil);
begin
  cxExportSchedulerToFile(AFileName, AScheduler, AsTable, AShowDialog,
    AHeader, AStart, AFinish, cxExportToExcel, [], AFileExt, AHandler);
end;

procedure cxExportSchedulerToXLSX(const AFileName: string; AScheduler: TcxCustomScheduler; AsTable: Boolean = False;
  AShowDialog: Boolean = False; const AHeader: string = 'Event %d'; const AStart: TDateTime = NullDate;
  const AFinish: TDateTime = NullDate; const AFileExt: string = 'xlsx'; AHandler: TObject = nil);
begin
  cxExportSchedulerToFile(AFileName, AScheduler, AsTable, AShowDialog,
    AHeader, AStart, AFinish, cxExportToXlsx, [], AFileExt, AHandler);
end;

procedure cxExportSchedulerToText(const AFileName: string; AScheduler: TcxCustomScheduler; AsTable: Boolean;
  AShowDialog: Boolean = False; const AStart: TDateTime = NullDate; const AFinish: TDateTime = NullDate;
  const AFileExt: string = 'txt'; AHandler: TObject = nil; AEncoding: TEncoding = nil); overload;
begin
  cxExportSchedulerToText(AFileName, AScheduler, AsTable, AShowDialog,
    'Event %d', AStart, AFinish, '', '', '', AFileExt, AHandler, AEncoding);
end;

procedure cxExportSchedulerToText(const AFileName: string; AScheduler: TcxCustomScheduler; AsTable: Boolean = False;
  AShowDialog: Boolean = False; const AHeader: string = 'Event %d'; const AStart: TDateTime = NullDate;
  const AFinish: TDateTime = NullDate; const ASeparator: string = ''; const ABeginString: string = '';
  const AEndString: string = ''; const AFileExt: string = 'txt'; AHandler: TObject = nil;
  AEncoding: TEncoding = nil); overload;
begin
  cxExportSchedulerToFile(AFileName, AScheduler, AsTable, AShowDialog, AHeader, AStart,
    AFinish, cxExportToText, [ASeparator, ABeginString, AEndString], AFileExt, AHandler, AEncoding);
end;

{ TfmExportRangeDialog }

constructor TfmExportRangeDialog.CreateEx(ALookAndFeel: TcxLookAndFeel; AStart, AFinish: TDateTime);
begin
  Create(Application);
  SetControlLookAndFeel(Self, ALookAndFeel);
  deStart.Date := AStart;
  deFinish.Date := AFinish;
  SetCaptions;
end;

procedure TfmExportRangeDialog.deDatePropertiesChange(Sender: TObject);
begin
  btnOk.Enabled := IsValid;
end;

procedure TfmExportRangeDialog.DoShow;
begin
  if UseSchedulerColorInDialogs then
    Color := GetFormColor;
end;

function TfmExportRangeDialog.GetFormColor: TColor;
begin
  Result := btnOk.LookAndFeel.Painter.DefaultSchedulerControlColor;
end;

function TfmExportRangeDialog.IsValid: Boolean;
var
  AStart, AFinish: TDateTime;
begin
  AStart := deStart.CurrentDate;
  AFinish := deFinish.CurrentDate;
  Result := (AStart <> NullDate) and (AFinish <> NullDate) and (AStart < AFinish);
end;

procedure TfmExportRangeDialog.SetCaptions;
begin
  Caption := cxGetResourceString(@secxSetDateRangeCaption);
  lbSetDateRange.Caption := cxGetResourceString(@secxSetDateRangeText);
  lbAnd.Caption := cxGetResourceString(@secxSetDateRangeAnd);
  btnOk.Caption := cxGetResourcestring(@scxOk);
  btnCancel.Caption := cxGetResourcestring(@scxCancel);
end;

end.
