{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPrnDlg;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Registry, ExtCtrls, StdCtrls, Menus, Buttons, ImgList, dxPSForm, dxPSESys,
  dxPSGlbl, dxPgsDlg, dxPrnDev, dxExtCtrls, dxCore, cxControls, cxContainer,
  cxEdit, cxLabel, cxLookAndFeelPainters, cxButtons, cxCheckBox, cxRadioGroup,
  cxGraphics, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxSpinEdit, cxListBox,
  cxGroupBox, cxLookAndFeels, IniFiles, cxClasses, dxLayoutContainer, dxLayoutControl,
  dxLayoutLookAndFeels, dxLayoutControlAdapters, dxLayoutcxEditAdapters, cxImageList;

type
  TdxPrintDlgButtonKind = (pdbPrinterProperties, pdbNetwork, pdbPreview,
    pdbPageSetup, pdbDefineStyles, pdbStyleOptions, pdbHelp);
  TdxPrintDlgButtons = set of TdxPrintDlgButtonKind;
  TdxPrintDlgOption = (pdoPrintToFile, pdoAllPages, pdoCurrentPage, pdoSelection,
    pdoPageRange, pdoPrintStyles);
  TdxPrintDlgOptions = set of TdxPrintDlgOption;

const
  pdbAll = [Low(TdxPrintDlgButtonKind)..High(TdxPrintDlgButtonKind)];
  pdbDefault = [pdbPrinterProperties, pdbNetwork, pdbPreview, pdbPageSetup,
    pdbDefineStyles, pdbStyleOptions];

  pdoAll = [Low(TdxPrintDlgOption)..High(TdxPrintDlgOption)];
  pdoDefaultOptionsEnabled = [pdoPrintToFile, pdoAllPages, pdoPageRange];
  pdoDefaultOptionsVisible = [pdoPrintToFile, pdoAllPages, pdoCurrentPage, pdoPageRange];

type
  TdxPageSetupEvent = procedure(Sender: TObject; var ADone: Boolean;
    APreviewBtnClicked, APrintBtnClicked: PBoolean) of object;

  PdxPrintDlgEvents = ^TdxPrintDlgEvents;
  TdxPrintDlgEvents = record
    OnClose: TNotifyEvent; {called on the OnHide event}
    OnPageSetup: TdxPageSetupEvent;
    OnShow: TNotifyEvent;
  end;

  PdxPrintDialogData = ^TdxPrintDialogData;
  TdxPrintDialogData = record
    Copies: Integer;
    Collate: Boolean;
    FileList: TStrings;
    FileName: string;
    MaxRange: Integer;
    MinRange: Integer;
    PageCount: Integer;
    PageNums: TdxPageNumbers;
    PageRanges: TdxPageRanges;
    Pages: string;
    PrintToFile: Boolean;
    StyleManager: TdxPrintStyleManager;
  end;

  PdxPrintDlgData = ^TdxPrintDlgData;
  TdxPrintDlgData = record
    DialogData: TdxPrintDialogData;
    Title: string;
    HelpContext: THelpContext;
    ButtonsEnabled: TdxPrintDlgButtons;
    ButtonsVisible: TdxPrintDlgButtons;
    OptionsEnabled: TdxPrintDlgOptions;
    OptionsVisible: TdxPrintDlgOptions;
    Events: TdxPrintDlgEvents;
    IsCheckUserInput: Boolean;
    PreviewBtnClicked: Boolean;
  end;

  TdxPrintDialog = class(TcxCustomComponent)
  private
    FButtonsEnabled: TdxPrintDlgButtons;
    FButtonsVisible: TdxPrintDlgButtons;
    FDialogData: TdxPrintDialogData;
    FHelpContext: THelpContext;
    FIsTitleAssigned: Boolean;
    FOptionsEnabled: TdxPrintDlgOptions;
    FOptionsVisible: TdxPrintDlgOptions;
    FPreviewBtnClicked: Boolean;
    FPrintBtnClicked: Boolean;
    FTitle: string;
    FUseFileList: Boolean;
    FOnClose: TNotifyEvent;
    FOnPageSetup: TdxPageSetupEvent;
    FOnShow: TNotifyEvent;
    function GetCollate: Boolean;
    function GetCopies: Integer;
    function GetFileList: TStrings;
    function GetFileName: string;
    function GetMaxRange: Integer;
    function GetMinRange: Integer;
    function GetPageCount: Integer;
    function GetPageNums: TdxPageNumbers;
    function GetPageRanges: TdxPageRanges;
    function GetPages: string;
    function GetPrintToFile: Boolean;
    function GetStyleManager: TdxPrintStyleManager;
    function GetTitle: string;
    function IsTitleStored: Boolean;
    procedure SetCollate(Value: Boolean);
    procedure SetCopies(Value: Integer);
    procedure SetFileList(Value: TStrings);
    procedure SetFileName(const Value: string);
    procedure SetMaxRange(Value: Integer);
    procedure SetMinRange(Value: Integer);
    procedure SetPageCount(Value: Integer);
    procedure SetPageNums(Value: TdxPageNumbers);
    procedure SetPageRanges(Value: TdxPageRanges);
    procedure SetPages(const Value: string);
    procedure SetPrintToFile(Value: Boolean);
    procedure SetStyleManager(Value: TdxPrintStyleManager);
    procedure SetTitle(const Value: string);

    procedure ReadIsTitleAssigned(Reader: TReader);
    procedure WriteIsTitleAssigned(Writer: TWriter);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function DefaultTitle: string; virtual;
    function Execute: Boolean;
    procedure RestoreDefaults; virtual;
    procedure SetMinMaxRanges(AMinRange, AMaxRange: Integer);

    property DialogData: TdxPrintDialogData read FDialogData;
    property FileList: TStrings read GetFileList write SetFileList;
    property PageCount: Integer read GetPageCount write SetPageCount;
    property PreviewBtnClicked: Boolean read FPreviewBtnClicked;
    property PrintBtnClicked: Boolean read FPrintBtnClicked;
  published
    property ButtonsEnabled: TdxPrintDlgButtons read FButtonsEnabled write FButtonsEnabled
      default [pdbPrinterProperties, pdbNetwork, pdbPreview, pdbPageSetup, pdbDefineStyles, pdbStyleOptions];
    property ButtonsVisible: TdxPrintDlgButtons read FButtonsVisible write FButtonsVisible
      default [pdbPrinterProperties, pdbNetwork, pdbPreview, pdbPageSetup, pdbDefineStyles, pdbStyleOptions];
    property Collate: Boolean read GetCollate write SetCollate default False;
    property Copies: Integer read GetCopies write SetCopies default 1;
    property FileName: string read GetFileName write SetFileName;
    property HelpContext: THelpContext read FHelpContext write FHelpContext default 0;
    property MaxRange: Integer read GetMaxRange write SetMaxRange default 1;
    property MinRange: Integer read GetMinRange write SetMinRange default 1;
    property OptionsEnabled: TdxPrintDlgOptions read FOptionsEnabled write FOptionsEnabled
      default [pdoPrintToFile, pdoAllPages, pdoPageRange];
    property OptionsVisible: TdxPrintDlgOptions read FOptionsVisible write FOptionsVisible
      default [pdoPrintToFile, pdoAllPages, pdoCurrentPage, pdoPageRange];
    property PageNums: TdxPageNumbers read GetPageNums write SetPageNums default pnAll;
    property PageRanges: TdxPageRanges read GetPageRanges write SetPageRanges default prAll;
    property Pages: string read GetPages write SetPages;
    property PrintToFile: Boolean read GetPrintToFile write SetPrintToFile default False;
    property StyleManager: TdxPrintStyleManager read GetStyleManager write SetStyleManager;
    property Title: string read GetTitle write SetTitle stored IsTitleStored;
    property UseFileList: Boolean read FUseFileList write FUseFileList default False;

    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnPageSetup: TdxPageSetupEvent read FOnPageSetup write FOnPageSetup;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;

  TdxfmPrintDialog = class(TCustomdxPSForm)
    pmPrintStyles: TPopupMenu;
    miPageSetup: TMenuItem;
    miDefineStyles: TMenuItem;
    miLine1: TMenuItem;
    ilPrinters: TcxImageList;
    pbxCollate: TPaintBox;
    cbxNumberOfPages: TcxComboBox;
    seCopies: TcxSpinEdit;
    btnPageSetup2: TcxButton;
    btnDefineStyles: TcxButton;
    lbxPrintStyles: TcxListBox;
    btnPageSetup: TcxButton;
    btnPreview: TcxButton;
    btnCancel: TcxButton;
    btnHelp: TcxButton;
    btnOK: TcxButton;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    libtnCancel: TdxLayoutItem;
    libtnHelp: TdxLayoutItem;
    libtnOK: TdxLayoutItem;
    libtnPreview: TdxLayoutItem;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    libtnPageSetup: TdxLayoutItem;
    gbxPrintStyles: TdxLayoutGroup;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    libtnDefineStyles: TdxLayoutItem;
    libtnPageSetup2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    gbxCopies: TdxLayoutGroup;
    lblNumberOfPages: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    lblNumberOfCopies: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    chbxCollate: TcxCheckBox;
    gbxPageRange: TdxLayoutGroup;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    lirbtnAllPages: TdxLayoutItem;
    rbtnAllPages: TcxRadioButton;
    lirbtnCurrentPage: TdxLayoutItem;
    rbtnCurrentPage: TcxRadioButton;
    lirbtnPageRanges: TdxLayoutItem;
    rbtnPageRanges: TcxRadioButton;
    liedPageRanges: TdxLayoutItem;
    edPageRanges: TcxTextEdit;
    dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup;
    lirbtnSelection: TdxLayoutItem;
    rbtnSelection: TcxRadioButton;
    dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup;
    bvlPRWarningHolder: TdxLayoutItem;
    gbxPrinter: TdxLayoutGroup;
    lblName: TdxLayoutItem;
    cbxPrinters: TcxComboBox;
    libtnPrinterProperties: TdxLayoutItem;
    btnPrinterProperties: TcxButton;
    libtnNetwork: TdxLayoutItem;
    btnNetwork: TcxButton;
    dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup;
    lblComment: TdxLayoutItem;
    lComment: TcxLabel;
    lblWhere: TdxLayoutItem;
    lWhere: TcxLabel;
    lblType: TdxLayoutItem;
    lType: TcxLabel;
    lblStatus: TdxLayoutItem;
    lStatus: TcxLabel;
    dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup;
    lichbxPrintToFile: TdxLayoutItem;
    chbxPrintToFile: TcxCheckBox;
    licbxFileName: TdxLayoutItem;
    cbxFileName: TcxComboBox;
    dxLayoutAutoCreatedGroup10: TdxLayoutAutoCreatedGroup;
    libtnBrowse: TdxLayoutItem;
    btnBrowse: TcxButton;
    lblDescription: TdxLayoutLabeledItem;
    procedure chbxCollateClick(Sender: TObject);
    procedure btnPrinterPropertiesClick(Sender: TObject);
    procedure cbxPrintersChange(Sender: TObject);
    procedure chbxPrintToFileClick(Sender: TObject);
    procedure edPageRangesChange(Sender: TObject);
    procedure cbxNumberOfPagesChange(Sender: TObject);
    procedure rbtnPagesClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure edPageRangesExit(Sender: TObject);
    procedure btnPreviewClick(Sender: TObject);
    procedure btnPageSetupClick(Sender: TObject);
    procedure edPageRangesKeyPress(Sender: TObject; var KEY: Char);
    procedure lblNumberOfPagesClick(Sender: TObject);
    procedure lblNumberOfCopiesClick(Sender: TObject);
    procedure btnNetworkClick(Sender: TObject);
    procedure seCopiesChange(Sender: TObject);
    procedure seCopiesExit(Sender: TObject);
    procedure PageSetup2Click(Sender: TObject);
    procedure lbxPrintStylesClick(Sender: TObject);
    procedure DefineStylesClick(Sender: TObject);
    procedure pmPrintStylesPopup(Sender: TObject);
    procedure cbxFileNameExit(Sender: TObject);
    procedure pbxCollatePaint(Sender: TObject);
    procedure pbxCollateDblClick(Sender: TObject);
    procedure cbxPrintersPropertiesDrawItem(AControl: TcxCustomComboBox;
      ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect; AState: TOwnerDrawState);
    procedure lbxPrintStylesDrawItem(AControl: TcxListBox; ACanvas: TcxCanvas;
      AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
    procedure btnHelpClick(Sender: TObject);
  private
    FControlsUpdating: Boolean;
    FDialogData: TdxPrintDialogData;
    FGlyphs: array[Boolean] of TBitmap;
    FIsCheckUserInput: Boolean;
    FModified: Boolean;
    FSubscriber: TdxEventSubscriber;
    FPreviewBtnClicked: Boolean;
    FPrintBtnClicked: Boolean;
    FSaveDialogData: TdxPrintDialogData;
    FPrintStylesVisibled: Boolean;
    FOnClose: TNotifyEvent;
    FOnPageSetup: TdxPageSetupEvent;
    FOnPrintDeviceChanged: TNotifyEvent;
    FOnPrintersChanged: TNotifyEvent;
    FOnShow: TNotifyEvent;
    function CheckFileName(const FileName: string): Boolean;
    procedure CheckModified;
    function CheckPageRanges: Boolean;
    function CheckUserInput: Boolean;
    procedure ConnectToPrinterDlg;
    procedure CreateControls;
    procedure DocumentPropertiesDlg;
    procedure DrawCollatedPages(DC: HDC; const ADrawRect: TRect; ACollate: Boolean);
    function GetFileName(const S: string): string;
    procedure RefreshPrinterList;
    procedure RefreshStyleList;
    procedure SavePrintDialogData;
    procedure SetActiveControl;
    procedure StartSettings;
    procedure StyleListChanged(Sender: TObject);
    procedure UpdateControlsState;
    procedure UpdatePrinterInfos;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  strict protected
    FwpPageRanges: TdxPSWarningPane;
  protected
    procedure CreateWnd; override;
    procedure DoHide; override;
    procedure DoShow; override;
    procedure LoadStrings; virtual;

    function CheckAvailablePrinters: Boolean;
    function DoShowStylePageSetup(AStyle: TBasedxPrintStyle; APageIndex: Integer; AShowPreviewBtn, AShowPrintBtn: Boolean;
      out APreviewBtnClicked, APrintBtnClicked: Boolean): Boolean; virtual;
    procedure CallOnPageSetup(ASender: TObject; var ADone: Boolean; APreviewBtnClicked, APrintBtnClicked: PBoolean); dynamic;
    procedure DefinePrintStyleDlg(out APreviewBtnClicked: Boolean); virtual;
    procedure DoPageSetup; dynamic;
    procedure PrepareSettings;
    procedure SetupDialog(const APrintDlgData: TdxPrintDlgData);
    procedure UpdatePrinters;

    property DialogData: TdxPrintDialogData read FDialogData write FDialogData;
    property OnPrintDeviceChanged: TNotifyEvent read FOnPrintDeviceChanged write FOnPrintDeviceChanged;
    property OnPrintersChanged: TNotifyEvent read FOnPrintersChanged write FOnPrintersChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    procedure LoadFromIniFile(AIniFile: TCustomIniFile; const ASectionName: string); override;
    procedure SaveToIniFile(AIniFile: TCustomIniFile; const ASectionName: string); override;
    procedure ShowPrintStyles(AShow: Boolean);
    //
    property PreviewBtnClicked: Boolean read FPreviewBtnClicked;
    property OnPageSetup: TdxPageSetupEvent read FOnPageSetup write FOnPageSetup;
  end;

function dxPrintDialog(var AData: TdxPrintDlgData): Boolean;

implementation

{$R *.DFM}

uses
  Variants, WinSpool, Math, Types, cxGeometry, dxPSPopupMan, dxPSRes, dxPSEngn, dxPSEvnt, dxPSUtl, dxPSImgs;

const
  sdxMaximized = 'Maximized'; // Don't Localize

function dxPrintDialog(var AData: TdxPrintDlgData): Boolean;
var
  Dialog: TdxfmPrintDialog;
begin
  Dialog := TdxfmPrintDialog.Create(nil);
  try
    Dialog.SetupDialog(AData);
    Result := Dialog.Execute;
    if Result then
      AData.DialogData := Dialog.FDialogData
    else
      AData.DialogData := Dialog.FSaveDialogData;
    AData.PreviewBtnClicked := Dialog.PreviewBtnClicked;
  finally
    Dialog.Free;
  end;
end;

{ Memory Management Routines }

function dxAllocMem(ASize: Integer): Pointer;
begin
  Result := AllocMem(ASize);
end;

procedure dxFreeMem(var P: Pointer; ASize: Integer);
begin
  FreeMem(P, ASize);
  P := nil;
end;

{ TdxPrintDialog }

constructor TdxPrintDialog.Create(AOwner: TComponent);
begin
  inherited;
  FillChar(FDialogData, SizeOf(TdxPrintDialogData), 0);
  FDialogData.Copies := 1;
  FDialogData.Collate := False;
  FDialogData.FileList := TStringList.Create;
  FDialogData.FileName := '';
  FDialogData.MaxRange := 1;
  FDialogData.MinRange := 1;
  FDialogData.PageCount := 0;
  FDialogData.PageNums := pnAll;
  FDialogData.PageRanges := prAll;
  FDialogData.Pages := '';
  FDialogData.PrintToFile := False;
  FDialogData.StyleManager := nil;

  FButtonsEnabled := pdbDefault;
  FOptionsEnabled := pdoDefaultOptionsEnabled;

  FButtonsVisible := pdbDefault;
  FOptionsVisible := pdoDefaultOptionsVisible;
end;

destructor TdxPrintDialog.Destroy;
begin
  FreeAndNil(FDialogData.FileList);
  inherited;
end;

function TdxPrintDialog.DefaultTitle: string;
begin
  Result := cxGetResourceString(@sdxPrintDialogCaption);
end;

function TdxPrintDialog.Execute: Boolean;
var
  APrintDlgData: TdxPrintDlgData;
  AEvents: TdxPrintDlgEvents;
  ADialogData: TdxPrintDialogData;
  ASaveStrings: TStrings;
begin
  Result := dxCheckAvailablePrinters;
  if Result then
  begin
    FillChar(APrintDlgData, SizeOf(TdxPrintDlgData), 0);
    FillChar(AEvents, SizeOf(TdxPrintDlgEvents), 0);
    FillChar(ADialogData, SizeOf(TdxPrintDialogData), 0);
    try
      ADialogData.Copies := Copies;
      ADialogData.Collate := Collate;
      if UseFileList then
      begin
        ADialogData.FileList := TStringList.Create;
        ADialogData.FileList.Assign(FileList);
      end;
      ADialogData.FileName := FileName;
      ADialogData.MaxRange := MaxRange;
      ADialogData.MinRange := MinRange;
      ADialogData.PageCount := PageCount;
      ADialogData.PageNums := PageNums;
      ADialogData.PageRanges := PageRanges;
      ADialogData.Pages := Pages;
      ADialogData.PrintToFile := PrintToFile;
      ADialogData.StyleManager := StyleManager;

      APrintDlgData.DialogData := ADialogData;
      APrintDlgData.HelpContext := HelpContext;
      APrintDlgData.Title := Title;
      APrintDlgData.IsCheckUserInput := not (csDesigning in ComponentState);
      APrintDlgData.OptionsEnabled := OptionsEnabled;
      APrintDlgData.OptionsVisible := OptionsVisible;
      APrintDlgData.ButtonsEnabled := ButtonsEnabled;
      APrintDlgData.ButtonsVisible := ButtonsVisible;

      AEvents.OnClose := OnClose;
      AEvents.OnPageSetup := OnPageSetup;
      AEvents.OnShow := OnShow;

      APrintDlgData.Events := AEvents;

      Result := dxPrintDialog(APrintDlgData);
      if Result then
      begin
        ASaveStrings := FileList;
        if UseFileList then
          FileList := APrintDlgData.DialogData.FileList;
        FDialogData := APrintDlgData.DialogData;
        FDialogData.FileList := ASaveStrings;
      end;
      FPreviewBtnClicked := APrintDlgData.PreviewBtnClicked;
    finally
      if ADialogData.FileList <> nil then ADialogData.FileList.Free;
    end;
  end;
end;

procedure TdxPrintDialog.RestoreDefaults;
begin
  ButtonsEnabled := pdbDefault;
  ButtonsVisible := pdbDefault;
  Collate := False;
  Copies := 1;
  MaxRange := 1;
  MinRange := 1;
  OptionsEnabled := pdoDefaultOptionsEnabled;
  OptionsVisible := pdoDefaultOptionsVisible;
  PageNums := pnAll;
  PageRanges := prAll;
  PrintToFile := False;
  UseFileList := False;
  FIsTitleAssigned := False;
end;

procedure TdxPrintDialog.SetMinMaxRanges(AMinRange, AMaxRange: Integer);
begin
  if AMinRange < 1 then AMinRange := 1;
  if AMaxRange <> -1 then
    if AMaxRange < AMinRange then AMaxRange := AMinRange;
  FDialogData.MinRange := AMinRange;
  FDialogData.MaxRange := AMaxRange;
end;

procedure TdxPrintDialog.AssignTo(Dest: TPersistent);

  procedure XorOption(var AOptions: TPrintDialogOptions; AItem: TPrintDialogOption; AValue: Boolean);
  begin
    if AValue then
      AOptions := AOptions + [AItem]
    else
      AOptions := AOptions - [AItem];
  end;

var
  PageIndexes: TIntegers;
  SrcOptions: TPrintDialogOptions;
begin
  if Dest is Dialogs.TPrintDialog then
    with Dialogs.TPrintDialog(Dest) do
    begin
      Collate := Self.Collate;
      Copies := Self.Copies;
      MinPage := Self.MinRange;
      MaxPage := Self.MaxRange;
      if DecodePageIndexes(Self.Pages, PageIndexes) then
      try
        FromPage := MinIntValue(PageIndexes);
        ToPage := MaxIntValue(PageIndexes);
      finally
        SetLength(PageIndexes, 0);
      end
      else
      begin
        FromPage := 1;
        ToPage := 1;
      end;

      SrcOptions := Options;
      XorOption(SrcOptions, poPrintToFile, pdoPrintToFile in Self.OptionsVisible);
      XorOption(SrcOptions, poDisablePrintToFile, pdoPrintToFile in Self.OptionsEnabled);
      XorOption(SrcOptions, poPageNums, (pdoPageRange in Self.OptionsVisible) and (pdoPageRange in Self.OptionsEnabled));
      XorOption(SrcOptions, poSelection, (pdoSelection in Self.OptionsVisible) and (pdoSelection in Self.OptionsEnabled));
      XorOption(SrcOptions, poHelp, (pdbHelp in Self.ButtonsVisible) and (pdbHelp in Self.ButtonsEnabled));
      Options := SrcOptions;

      PrintToFile := Self.PrintToFile;

      if Self.PageRanges = prSelection then
        PrintRange := Dialogs.prAllPages
      else
        PrintRange := TPrintRange(PageRanges);
    end;
end;

procedure TdxPrintDialog.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('IsTitleAssigned', ReadIsTitleAssigned, WriteIsTitleAssigned,
    FIsTitleAssigned and (Title = ''));
end;

procedure TdxPrintDialog.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = StyleManager) and (Operation = opRemove) then
    StyleManager := nil;
end;

function TdxPrintDialog.GetPageCount: Integer;
begin
  Result := FDialogData.PageCount;
end;

function TdxPrintDialog.GetCollate: Boolean;
begin
  Result := FDialogData.Collate;
end;

function TdxPrintDialog.GetCopies: Integer;
begin
  Result := FDialogData.Copies;
end;

function TdxPrintDialog.GetFileList: TStrings;
begin
  Result := FDialogData.FileList;
end;

function TdxPrintDialog.GetFileName: string;
begin
  Result := FDialogData.FileName;
end;

function TdxPrintDialog.GetMaxRange: Integer;
begin
  Result := FDialogData.MaxRange;
end;

function TdxPrintDialog.GetMinRange: Integer;
begin
  Result := FDialogData.MinRange;
end;

function TdxPrintDialog.GetPageNums: TdxPageNumbers;
begin
  Result := FDialogData.PageNums;
end;

function TdxPrintDialog.GetPageRanges: TdxPageRanges;
begin
  Result := FDialogData.PageRanges;
end;

function TdxPrintDialog.GetPages: string;
begin
  Result := FDialogData.Pages;
end;

function TdxPrintDialog.GetPrintToFile: Boolean;
begin
  Result := FDialogData.PrintToFile;
end;

function TdxPrintDialog.GetStyleManager: TdxPrintStyleManager;
begin
  Result := FDialogData.StyleManager;
end;

function TdxPrintDialog.GetTitle: string;
begin
  if FIsTitleAssigned then
    Result := FTitle
  else
    Result := DefaultTitle;
end;

function TdxPrintDialog.IsTitleStored: Boolean;
begin
  Result := FIsTitleAssigned and (Title <> DefaultTitle);
end;

procedure TdxPrintDialog.SetCollate(Value: Boolean);
begin
  FDialogData.Collate := Value;
end;

procedure TdxPrintDialog.SetCopies(Value: Integer);
begin
  FDialogData.Copies := Value;
end;

procedure TdxPrintDialog.SetFileList(Value: TStrings);
begin
  FDialogData.FileList.Assign(Value);
end;

procedure TdxPrintDialog.SetFileName(const Value: string);
begin
  FDialogData.FileName := Value;
end;

procedure TdxPrintDialog.SetMaxRange(Value: Integer);
begin
  SetMinMaxRanges(MinRange, Value);
end;

procedure TdxPrintDialog.SetMinRange(Value: Integer);
begin
  SetMinMaxRanges(Value, MaxRange);
end;

procedure TdxPrintDialog.SetPageCount(Value: Integer);
begin
  if Value < 1 then Value := 1;
  FDialogData.PageCount := Value;
end;

procedure TdxPrintDialog.SetPageNums(Value: TdxPageNumbers);
begin
  FDialogData.PageNums := Value;
end;

procedure TdxPrintDialog.SetPageRanges(Value: TdxPageRanges);
begin
  if FDialogData.PageRanges <> Value then
  begin
    case Value of
      prAll:
        begin
          OptionsVisible := OptionsVisible + [pdoAllPages];
          OptionsEnabled := OptionsEnabled + [pdoAllPages];
        end;
      prCurrent:
        begin
          OptionsVisible := OptionsVisible + [pdoCurrentPage];
          OptionsEnabled := OptionsEnabled + [pdoCurrentPage];
        end;
      prRange:
        begin
          OptionsVisible := OptionsVisible + [pdoPageRange];
          OptionsEnabled := OptionsEnabled + [pdoPageRange];
        end;
      prSelection:
        begin
          OptionsVisible := OptionsVisible + [pdoSelection];
          OptionsEnabled := OptionsEnabled + [pdoSelection];
        end;
    end;
    FDialogData.PageRanges := Value;
  end;
end;

procedure TdxPrintDialog.SetPages(const Value: string);
begin
  FDialogData.Pages := Value;
end;

procedure TdxPrintDialog.SetPrintToFile(Value: Boolean);
begin
  FDialogData.PrintToFile := Value;
end;

procedure TdxPrintDialog.SetStyleManager(Value: TdxPrintStyleManager);
begin
  if FDialogData.StyleManager <> Value then
  begin
    FDialogData.StyleManager := Value;
    if StyleManager <> nil then
      StyleManager.FreeNotification(Self);
  end;
end;

procedure TdxPrintDialog.SetTitle(const Value: string);
begin
  if Title <> Value then
  begin
    FTitle := Value;
    FIsTitleAssigned := True;
  end;
end;

procedure TdxPrintDialog.ReadIsTitleAssigned(Reader: TReader);
begin
  FIsTitleAssigned := Reader.ReadBoolean;
end;

procedure TdxPrintDialog.WriteIsTitleAssigned(Writer: TWriter);
begin
  Writer.WriteBoolean(FIsTitleAssigned);
end;

{ utilities }

function GetStatusString(Status: DWORD): string;
begin
  case Status of
    0:
      Result := cxGetResourceString(@sdxPrintDialogPSReady);
    PRINTER_STATUS_PAUSED:
      Result := cxGetResourceString(@sdxPrintDialogPSPaused);
    PRINTER_STATUS_PENDING_DELETION:
      Result := cxGetResourceString(@sdxPrintDialogPSPendingDeletion);
    PRINTER_STATUS_BUSY:
      Result := cxGetResourceString(@sdxPrintDialogPSBusy);
    PRINTER_STATUS_DOOR_OPEN:
      Result := cxGetResourceString(@sdxPrintDialogPSDoorOpen);
    PRINTER_STATUS_ERROR:
      Result := cxGetResourceString(@sdxPrintDialogPSError);
    PRINTER_STATUS_INITIALIZING:
      Result := cxGetResourceString(@sdxPrintDialogPSInitializing);
    PRINTER_STATUS_IO_ACTIVE:
      Result := cxGetResourceString(@sdxPrintDialogPSIOActive);
    PRINTER_STATUS_MANUAL_FEED:
      Result := cxGetResourceString(@sdxPrintDialogPSManualFeed);
    PRINTER_STATUS_NO_TONER:
      Result := cxGetResourceString(@sdxPrintDialogPSNoToner);
    PRINTER_STATUS_NOT_AVAILABLE:
      Result := cxGetResourceString(@sdxPrintDialogPSNotAvailable);
    PRINTER_STATUS_OFFLINE:
      Result := cxGetResourceString(@sdxPrintDialogPSOFFLine);
    PRINTER_STATUS_OUT_OF_MEMORY:
      Result := cxGetResourceString(@sdxPrintDialogPSOutOfMemory);
    PRINTER_STATUS_OUTPUT_BIN_FULL:
      Result := cxGetResourceString(@sdxPrintDialogPSOutBinFull);
    PRINTER_STATUS_PAGE_PUNT:
      Result := cxGetResourceString(@sdxPrintDialogPSPagePunt);
    PRINTER_STATUS_PAPER_JAM:
      Result := cxGetResourceString(@sdxPrintDialogPSPaperJam);
    PRINTER_STATUS_PAPER_OUT:
      Result := cxGetResourceString(@sdxPrintDialogPSPaperOut);
    PRINTER_STATUS_PAPER_PROBLEM:
      Result := cxGetResourceString(@sdxPrintDialogPSPaperProblem);
    PRINTER_STATUS_PRINTING:
      Result := cxGetResourceString(@sdxPrintDialogPSPrinting);
    PRINTER_STATUS_PROCESSING:
      Result := cxGetResourceString(@sdxPrintDialogPSProcessing);
    PRINTER_STATUS_TONER_LOW:
      Result := cxGetResourceString(@sdxPrintDialogPSTonerLow);
    PRINTER_STATUS_USER_INTERVENTION:
      Result := cxGetResourceString(@sdxPrintDialogPSUserIntervention);
    PRINTER_STATUS_WAITING:
      Result := cxGetResourceString(@sdxPrintDialogPSWaiting);
    PRINTER_STATUS_WARMING_UP:
      Result := cxGetResourceString(@sdxPrintDialogPSWarningUp);
  else
    Result := '';
  end;
end;

{ TfmdxPrintDialog }

constructor TdxfmPrintDialog.Create(AOwner: TComponent);
begin
  inherited;
  HelpContext := dxPSGlbl.dxhcPrintDlg;
  FillChar(FDialogData, SizeOf(TdxPrintDialogData), 0);
  FillChar(FSaveDialogData, SizeOf(TdxPrintDialogData), 0);
  CreateControls;
  FGlyphs[False] := CreateDoubleArrowBitmap(udgUp, ScaleFactor.Apply(8));
  FGlyphs[True] := CreateDoubleArrowBitmap(udgDown, ScaleFactor.Apply(8));

  pmPrintStyles.Images := ilPrinters;
  miPageSetup.ImageIndex := 4;
  miDefineStyles.ImageIndex := 5;

  FSubscriber := TdxStyleListChangedSubscriber.Create([TdxSMStyleListChangedEvent]);
  TdxStyleListChangedSubscriber(FSubscriber).OnStyleListChanged := StyleListChanged;
  dxPSPopupMenuController.RegisterControl(lbxPrintStyles);
end;

destructor TdxfmPrintDialog.Destroy;
begin
  dxPSPopupMenuController.UnregisterControl(lbxPrintStyles);
  FreeAndNil(FSubscriber);
  FreeAndNil(FGlyphs[True]);
  FreeAndNil(FGlyphs[False]);
  inherited;
end;

function TdxfmPrintDialog.Execute: Boolean;
begin
  Result := CheckAvailablePrinters;
  if Result then
  begin
    PrepareSettings;
    Result := ShowModal = mrOk;
  end;
end;

procedure TdxfmPrintDialog.CreateWnd;
begin
  inherited;
  if Icon.Handle = 0 then
    dxLoadIconFromResource(Icon, IDB_DXPSPRINT);
  SendMessage(Handle, WM_SETICON, 1, Icon.Handle);
end;

procedure TdxfmPrintDialog.DoHide;
begin
  if Assigned(FOnClose) then FOnClose(Self);
  inherited;
end;

procedure TdxfmPrintDialog.DoShow;
begin
  inherited;
  if Assigned(FOnShow) then FOnShow(Self);
end;

procedure TdxfmPrintDialog.LoadStrings;
var
  Index: Integer;
begin
  Caption := cxGetResourceString(@sdxPrintDialogCaption);
  gbxPrinter.Caption := cxGetResourceString(@sdxPrintDialogPrinter);

  lblName.Caption :=  cxGetResourceString(@sdxPrintDialogName);
  lblStatus.Caption := cxGetResourceString(@sdxPrintDialogStatus);
  lblType.Caption := cxGetResourceString(@sdxPrintDialogType);
  lblWhere.Caption := cxGetResourceString(@sdxPrintDialogWhere);
  lblComment.Caption := cxGetResourceString(@sdxPrintDialogComment);
  chbxPrintToFile.Caption := cxGetResourceString(@sdxPrintDialogPrintToFile);

  gbxPageRange.Caption := cxGetResourceString(@sdxPrintDialogPageRange);
  rbtnAllPages.Caption := cxGetResourceString(@sdxPrintDialogAll);
  rbtnCurrentPage.Caption := cxGetResourceString(@sdxPrintDialogCurrentPage);
  rbtnSelection.Caption := cxGetResourceString(@sdxPrintDialogSelection);
  rBtnPageRanges.Caption := cxGetResourceString(@sdxPrintDialogPages);
  lblDescription.Caption := cxGetResourceString(@sdxPrintDialogRangeLegend);
  gbxCopies.Caption := cxGetResourceString(@sdxPrintDialogCopies);

  lblNumberOfPages.Caption := cxGetResourceString(@sdxPrintDialogNumberOfPages);
  Index := cbxNumberOfPages.ItemIndex;
  with cbxNumberOfPages.Properties.Items do
  begin
    BeginUpdate;
    try
      Clear;
      Add(cxGetResourceString(@sdxPrintDialogAllPages));
      Add(cxGetResourceString(@sdxPrintDialogEvenPages));
      Add(cxGetResourceString(@sdxPrintDialogOddPages));
    finally
      EndUpdate;
    end;
  end;
  cbxNumberOfPages.ItemIndex := Index;

  lblNumberOfCopies.Caption := cxGetResourceString(@sdxPrintDialogNumberOfCopies);
  chbxCollate.Caption := cxGetResourceString(@sdxPrintDialogCollateCopies);

  btnPrinterProperties.Caption := cxGetResourceString(@sdxBtnProperties);
  btnNetwork.Caption := cxGetResourceString(@sdxBtnNetwork);
  btnBrowse.Caption := cxGetResourceString(@sdxBtnBrowse);
  btnPageSetup.Caption := cxGetResourceString(@sdxBtnPageSetup);
  btnPreview.Caption := cxGetResourceString(@sdxBtnPreview);
  btnOK.Caption := DropEndEllipsis(cxGetResourceString(@sdxBtnPrint));
  btnCancel.Caption := cxGetResourceString(@sdxBtnCancel);
  btnHelp.Caption := cxGetResourceString(@sdxBtnHelp);

  gbxPrintStyles.Caption := cxGetResourceString(@sdxPrintDialogPrintStyles);
  btnPageSetup2.Caption := cxGetResourceString(@sdxBtnPageSetup);
  btnDefineStyles.Caption := cxGetResourceString(@sdxBtnDefinePrintStyles);
  miPageSetup.Caption := cxGetResourceString(@sdxBtnPageSetup);
  miDefineStyles.Caption := cxGetResourceString(@sdxBtnDefinePrintStyles);
end;

function TdxfmPrintDialog.CheckAvailablePrinters: Boolean;
begin
  Result := dxCheckAvailablePrinters;
end;

function TdxfmPrintDialog.DoShowStylePageSetup(AStyle: TBasedxPrintStyle; APageIndex: Integer;
  AShowPreviewBtn, AShowPrintBtn: Boolean; out APreviewBtnClicked, APrintBtnClicked: Boolean): Boolean;
begin
  Result := AStyle.PageSetup(APageIndex, AShowPreviewBtn, AShowPrintBtn, APreviewBtnClicked, APrintBtnClicked);
end;

procedure TdxfmPrintDialog.CallOnPageSetup(ASender: TObject; var ADone: Boolean;
  APreviewBtnClicked, APrintBtnClicked: PBoolean);
begin
  FOnPageSetup(ASender, ADone, APreviewBtnClicked, APreviewBtnClicked);
end;

procedure TdxfmPrintDialog.DefinePrintStyleDlg(out APreviewBtnClicked: Boolean);
var
  PrintBtnClicked: Boolean;
begin
  FDialogData.StyleManager.DefinePrintStylesDlg(APreviewBtnClicked, PrintBtnClicked);
end;

procedure TdxfmPrintDialog.SetupDialog(const APrintDlgData: TdxPrintDlgData);
begin
//  FModified := False;
  FControlsUpdating := True;
  try
    FSaveDialogData := APrintDlgData.DialogData;
    FDialogData := FSaveDialogData;

    if APrintDlgData.HelpContext <> 0 then
      HelpContext := APrintDlgData.HelpContext
    else
      HelpContext := dxPSGlbl.dxhcPrintDlg;

    FIsCheckUserInput := APrintDlgData.IsCheckUserInput;

    with APrintDlgData do
    begin
      { visible }
      libtnPrinterProperties.Visible := pdbPrinterProperties in ButtonsVisible;
      libtnNetwork.Visible := (pdbNetwork in ButtonsVisible) and not IsWin95;
      lichbxPrintToFile.Visible := pdoPrintToFile in OptionsVisible;
      licbxFileName.Visible := pdoPrintToFile in OptionsVisible;
      libtnBrowse.Visible := pdoPrintToFile in OptionsVisible;

      lirbtnAllPages.Visible := pdoAllPages in OptionsVisible;
      lirbtnCurrentPage.Visible := pdoCurrentPage in OptionsVisible;
      lirbtnSelection.Visible := pdoSelection in OptionsVisible;
      lirbtnPageRanges.Visible := pdoPageRange in OptionsVisible;
      lblDescription.Visible := lirbtnPageRanges.Visible;
      liedPageRanges.Visible := pdoPageRange in OptionsVisible;
      gbxPageRange.Visible := lirbtnAllPages.Visible or lirbtnCurrentPage.Visible or lirbtnSelection.Visible or lirbtnPageRanges.Visible;

      libtnPageSetup2.Visible := pdbPageSetup in ButtonsVisible;
      libtnDefineStyles.Visible := pdbDefineStyles in ButtonsVisible;

      libtnPageSetup.Visible := pdbPageSetup in ButtonsVisible;
      libtnPreview.Visible := pdbPreview in ButtonsVisible;
      libtnHelp.Visible := pdbHelp in ButtonsVisible;

      if (DialogData.StyleManager = nil) or not (pdoPrintStyles in OptionsVisible) then
        gbxPrintStyles.Visible := False;

      {enable}
      if libtnPrinterProperties.Visible then
        btnPrinterProperties.Enabled := pdbPrinterProperties in ButtonsEnabled;
      if libtnNetwork.Visible then
        btnNetwork.Enabled := dxPSUtl.IsNetworkPresent and (pdbNetwork in ButtonsEnabled);
      if lichbxPrintToFile.Visible then
        chbxPrintToFile.Enabled := pdoPrintToFile in OptionsEnabled;
      if licbxFileName.Visible then
      begin
        cbxFileName.Enabled := pdoPrintToFile in OptionsEnabled;
        if DialogData.FileList <> nil then
          cbxFileName.Properties.Items := DialogData.FileList;
        cbxFileName.ItemIndex := 0;
      end;
      if libtnBrowse.Visible then
        btnBrowse.Enabled := pdoPrintToFile in OptionsEnabled;
      if lirbtnAllPages.Visible then
        rbtnAllPages.Enabled := pdoAllPages in OptionsEnabled;
      if lirbtnCurrentPage.Visible then
        rbtnCurrentPage.Enabled := pdoCurrentPage in OptionsEnabled;
      if lirbtnSelection.Visible then
        rbtnSelection.Enabled := pdoSelection in OptionsEnabled;
      if lirBtnPageRanges.Visible then
        rBtnPageRanges.Enabled := pdoPageRange in OptionsEnabled;
      if liedPageRanges.Visible then
        edPageRanges.Enabled := pdoPageRange in OptionsEnabled;
      if lblDescription.Visible then
        lblDescription.Enabled := rBtnPageRanges.Enabled;
      if edPageRanges.Enabled and liedPageRanges.Visible then
        edPageRanges.Text := DialogData.Pages;

      if libtnPageSetup.Visible then
        btnPageSetup.Enabled := pdbPageSetup in ButtonsEnabled;
      if libtnPreview.Visible then
        btnPreview.Enabled := pdbPreview in ButtonsEnabled;
      if libtnHelp.Visible then
        btnHelp.Enabled := pdbHelp in ButtonsEnabled;
      Caption := Title;

      FOnClose := Events.OnClose;
      FOnShow := Events.OnShow;
      FOnPageSetup := Events.OnPageSetup;
    end;
  finally
    UpdateControlsState;
    FControlsUpdating := False;
  end;
end;

procedure TdxfmPrintDialog.DoPageSetup;
var
  Done: Boolean;
begin
  if Assigned(FOnPageSetup) then
  begin
    Done := True;
    try
      CallOnPageSetup(Self, Done, @FPreviewBtnClicked, nil);//@FPrintBtnClicked);
    except
      Application.HandleException(Self);
    end;
    if Done then
    begin
      UpdatePrinterInfos;
      CheckModified;
      btnCancel.Caption := cxGetResourceString(@sdxBtnClose);
    end;
    if FPreviewBtnClicked then ModalResult := mrOK;
  end;
end;

procedure TdxfmPrintDialog.PrepareSettings;
begin
  StartSettings;
  ShowPrintStyles(FPrintStylesVisibled and (FDialogData.StyleManager <> nil));
  if FDialogData.StyleManager <> nil then
    TdxStyleListChangedSubscriber(FSubscriber).StyleListChanged(FDialogData.StyleManager);
  SetActiveControl;
end;

procedure TdxfmPrintDialog.UpdatePrinters;
begin
  RefreshPrinterList;
  UpdatePrinterInfos;
  if dxPrintDevice.Printers.Count > 0 then
    dxPrintDevice.PrinterIndex := -1
  else
    cbxPrinters.Enabled := False;
  Invalidate;
end;

procedure TdxfmPrintDialog.chbxCollateClick(Sender: TObject);
begin
  if FControlsUpdating then Exit;
  FDialogData.Collate := TcxCheckBox(Sender).Checked;
  pbxCollate.Invalidate;
  CheckModified;
end;

procedure TdxfmPrintDialog.btnPrinterPropertiesClick(Sender: TObject);
begin
  if dxPrintDevice.Printing then
    MessageWarning(cxGetResourceString(@sdxPrintDialogInPrintingState))
  else
    DocumentPropertiesDlg;
end;

procedure TdxfmPrintDialog.cbxPrintersChange(Sender: TObject);
var
  PrevCursor: TCursor;
begin
  if FControlsUpdating then Exit;
  PrevCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    dxPrintDevice.PrinterIndex := TcxComboBox(Sender).ItemIndex;
  finally
    Screen.Cursor := PrevCursor;
  end;
  btnCancel.Caption := cxGetResourceString(@sdxBtnClose);
  UpdatePrinterInfos;
  CheckModified;
  dxCallNotify(OnPrintersChanged, Self);
end;

procedure TdxfmPrintDialog.chbxPrintToFileClick(Sender: TObject);
begin
  if FControlsUpdating then Exit;
  FDialogData.PrintToFile := TcxCheckBox(Sender).Checked;
  if TcxCheckBox(Sender).Checked and (cbxFileName.Properties.Items.Count > 0) then
    cbxFileName.ItemIndex := 0;
  CheckModified;
  if TcxCheckBox(Sender).Checked then
    ActiveControl := btnBrowse;
end;

procedure TdxfmPrintDialog.edPageRangesChange(Sender: TObject);
begin
  if not FControlsUpdating then
  begin
    FControlsUpdating := True;
    try
      if edPageRanges.Focused then
      begin
        rBtnPageRanges.Checked := True;
        FDialogData.PageRanges := prRange;
      end;
      CheckModified;
    finally
      FControlsUpdating := False;
    end;
  end;
end;

procedure TdxfmPrintDialog.cbxNumberOfPagesChange(Sender: TObject);
begin
  if FControlsUpdating then Exit;
  FDialogData.PageNums := TdxPageNumbers(TcxComboBox(Sender).ItemIndex);
  CheckModified;
end;

procedure TdxfmPrintDialog.seCopiesChange(Sender: TObject);
begin
  if FControlsUpdating then Exit;
  CheckModified;
end;

procedure TdxfmPrintDialog.seCopiesExit(Sender: TObject);
begin
  FDialogData.Copies := TcxSpinEdit(Sender).Value;
end;

procedure TdxfmPrintDialog.rbtnPagesClick(Sender: TObject);
begin
  if FControlsUpdating then Exit;
  FDialogData.PageRanges := TdxPageRanges(TTagToInt(TcxRadioButton(Sender).Tag));
  CheckModified;
  if Sender = rbtnPageRanges then
  begin
    if ActiveControl <> edPageRanges then
    begin
      ActiveControl := edPageRanges;
      edPageRanges.SelectAll;
    end;
  end
  else
  begin
    edPageRanges.Text := '';
    FwpPageRanges.State := False;
  end;
end;

procedure TdxfmPrintDialog.btnBrowseClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    with OpenDialog do
    begin
      Title := cxGetResourceString(@sdxPrintDialogOpenDlgTitle);
      Filter := Format('%s (*.*)|*.*|%s (*.prn)|*.PRN',
        [cxGetResourceString(@sdxPrintDialogOpenDlgAllFiles),
         cxGetResourceString(@sdxPrintDialogOpenDlgPrinterFiles)]);
      FilterIndex := 2;
      DefaultExt := 'prn';
      FileName := cbxFileName.Text;
      if Execute then
        cbxFileName.Text := FileName;
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TdxfmPrintDialog.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrOk then
  begin
    if FIsCheckUserInput then
      CanClose := CheckUserInput;
    if CanClose then
      SavePrintDialogData;
  end;
end;

procedure TdxfmPrintDialog.cbxFileNameExit(Sender: TObject);
var
  S: string;
begin
  S := StringReplace(TcxComboBox(Sender).Text, ' ', '', [rfReplaceAll, rfIgnoreCase]);
  FDialogData.FileName := GetFileName(S);
end;

procedure TdxfmPrintDialog.edPageRangesExit(Sender: TObject);
var
  b: Boolean;
begin
  with TcxTextEdit(Sender) do
    Text := StringReplace(Text, ' ', '', [rfReplaceAll, rfIgnoreCase]);
  FDialogData.Pages := TcxTextEdit(Sender).Text;
  if rbtnAllPages.Checked or rbtnCurrentPage.Checked or rbtnSelection.Checked or
    (rbtnAllPages = ActiveControl) or (rbtnCurrentPage = ActiveControl) or
    (rbtnSelection = ActiveControl)
  then
    FwpPageRanges.State := False
  else
  begin
    try
      // order is very important because there is a possibility of rising exception in CheckPageRanges
      b := CheckPageRanges or (FDialogData.MaxRange = -1);
      if not b then
        FwpPageRanges.Hint := Format(cxGetResourceString(@sdxPrintDialogPageNumbersOutOfRange),
          [FDialogData.MinRange, FDialogData.MaxRange]);
    except
      b := False;
      FwpPageRanges.Hint := Format(cxGetResourceString(@sdxPrintDialogInvalidPageRanges), [edPageRanges.Text]);
    end;
    if not b and not FwpPageRanges.State and (ActiveControl <> btnOK) and (ActiveControl <> btnCancel) then
      Beep;
    FwpPageRanges.State := not b;
  end;
end;

procedure TdxfmPrintDialog.edPageRangesKeyPress(Sender: TObject; var Key: Char);

  function IsValidKey(AKey: Char): Boolean;
  begin
    Result := dxCharInSet(AKey, ['0'..'9']);
    if not Result and (Text <> '') then
      Result := (AKey = cPageSeparator) or (AKey = cPageRangeSeparator) or (AKey = Char(VK_BACK));
  end;

begin
  if not IsValidKey(Key) then
  begin
    MessageBeep(MB_ICONHAND);
    Key := #0;
  end;
end;

procedure TdxfmPrintDialog.btnPreviewClick(Sender: TObject);
begin
  FPreviewBtnClicked := True;
  ModalResult := mrCancel; // mrOK ???
end;

procedure TdxfmPrintDialog.ShowPrintStyles(AShow: Boolean);
begin
  if gbxPrintStyles.Visible <> AShow then
  begin
    gbxPrintStyles.Visible := AShow;
    btnPageSetup.Glyph.Assign(FGlyphs[not gbxPrintStyles.Visible]);
  end;
end;

procedure TdxfmPrintDialog.btnPageSetupClick(Sender: TObject);
begin
  if FDialogData.StyleManager <> nil then
    ShowPrintStyles(not gbxPrintStyles.Visible)
  else
    DoPageSetup;
end;

procedure TdxfmPrintDialog.lblNumberOfPagesClick(Sender: TObject);
begin
  ActivateComboBoxControl(Self, TcxLabel(Sender).FocusControl);
end;

procedure TdxfmPrintDialog.lblNumberOfCopiesClick(Sender: TObject);
begin
  TcxLabel(Sender).FocusControl.SetFocus;
end;

procedure TdxfmPrintDialog.btnHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TdxfmPrintDialog.btnNetworkClick(Sender: TObject);
begin
  Self.ConnectToPrinterDlg;
  UpdateControlsState;
end;

procedure TdxfmPrintDialog.lbxPrintStylesClick(Sender: TObject);
begin
  with lbxPrintStyles do
    FDialogData.StyleManager.CurrentStyle := TBasedxPrintStyle(Items.Objects[ItemIndex]);
  btnCancel.Caption := cxGetResourceString(@sdxBtnClose);
end;

procedure TdxfmPrintDialog.DefineStylesClick(Sender: TObject);
begin
  DefinePrintStyleDlg(FPreviewBtnClicked);
  btnCancel.Caption := cxGetResourceString(@sdxBtnClose);
  if FPreviewBtnClicked then ModalResult := mrOK;
end;

procedure TdxfmPrintDialog.PageSetup2Click(Sender: TObject);
var
  Index: Integer;
  Style: TBasedxPrintStyle;
begin
  Index := lbxPrintStyles.ItemIndex;
  if Index <> -1 then
  begin
    Style := TBasedxPrintStyle(lbxPrintStyles.Items.Objects[Index]);
    if DoShowStylePageSetup(Style, 0, True, True, FPreviewBtnClicked, FPrintBtnClicked) then
      btnCancel.Caption := cxGetResourceString(@sdxBtnClose);
    if FPreviewBtnClicked then
      ModalResult := mrOK;
  end;
end;

procedure TdxfmPrintDialog.pmPrintStylesPopup(Sender: TObject);
begin
  miPageSetup.Enabled := btnPageSetup2.Enabled;
end;

procedure TdxfmPrintDialog.LoadFromIniFile(
  AIniFile: TCustomIniFile; const ASectionName: string);
begin
  inherited LoadFromIniFile(AIniFile, ASectionName);
  FPrintStylesVisibled := AIniFile.ReadBool(ASectionName, sdxMaximized, FPrintStylesVisibled);
end;

procedure TdxfmPrintDialog.SaveToIniFile(
  AIniFile: TCustomIniFile; const ASectionName: string);
begin
  inherited SaveToIniFile(AIniFile, ASectionName);
  AIniFile.WriteBool(ASectionName, sdxMaximized, FPrintStylesVisibled);
end;

procedure TdxfmPrintDialog.pbxCollateDblClick(Sender: TObject);
begin
  chbxCollate.Checked := not chbxCollate.Checked;
end;

procedure TdxfmPrintDialog.pbxCollatePaint(Sender: TObject);
var
  DC: HDC;
  R: TRect;
begin
  with TPaintBox(Sender) do
  begin
    DC := Canvas.Handle;
    SelectObject(DC, Font.Handle);
    SetTextColor(DC, GetSysColor(COLOR_WINDOWTEXT));
    R := ClientRect;
  end;
  SetBkMode(DC, TRANSPARENT);
  DrawCollatedPages(DC, R, FDialogData.Collate);
  SelectClipRgn(DC, 0);
end;

function TdxfmPrintDialog.CheckFileName(const FileName: string): Boolean;
begin
  Result := ValidateFileName(FileName)
end;

procedure TdxfmPrintDialog.CheckModified;
begin
  if not FModified then FModified := True;
  UpdateControlsState;
end;

function TdxfmPrintDialog.CheckPageRanges: Boolean;
var
  PageIndexes: TIntegers;
begin
  Result := DecodePageIndexes(edPageRanges.Text, PageIndexes) and
     (MinIntValue(PageIndexes) >= FDialogData.MinRange) and
     (MaxIntValue(PageIndexes) <= FDialogData.MaxRange);
end;

function TdxfmPrintDialog.CheckUserInput: Boolean;
var
  FileName: string;
  RealFileName: string;
begin
  Result := True;
  if chbxPrintToFile.Checked then
  begin
    FileName := cbxFileName.Text;
    Result := Length(Filename) > 0;
    if Result then
    begin
      if (Length(FileName) > 2) and (FileName[1] = '"') and (FileName[Length(FileName)] = '"') then
        FileName := Copy(FileName, 2, Length(FileName) - 2);
      Result := CheckFileName(Filename);
      if Result then
      begin
        RealFileName := GetFileName(cbxFileName.Text);
        Result := not FileExists(RealFileName);
        if not Result then
          Result := MessageQuestion(Format(cxGetResourceString(@sdxConfirmOverWrite), [RealFileName]))
      end
      else
        MessageWarning(Format(cxGetResourceString(@sdxInvalidFileName), [FileName]));
    end
    else
      MessageWarning(cxGetResourceString(@sdxRequiredFileName));
    if not Result then
      ActiveControl := cbxFileName;
    Exit;
  end;
  if rBtnPageRanges.Checked then
  begin
    Result := Length(edPageRanges.Text) > 0;
    if Result then
    try
      // Because there is possibility of exception raising in CheckPageRanges that order is very important
      Result := CheckPageRanges or (FDialogData.MaxRange = -1);
      if not Result then
        MessageWarning(Format(cxGetResourceString(@sdxPrintDialogPageNumbersOutOfRange),
          [FDialogData.MinRange, FDialogData.MaxRange]));
    except
      MessageWarning(Format(cxGetResourceString(@sdxPrintDialogInvalidPageRanges), [edPageRanges.Text]));
      Result := False;
    end
    else
      MessageWarning(cxGetResourceString(@sdxPrintDialogRequiredPageNumbers));
    if not Result then
      ActiveControl := edPageRanges;
  end;
end;

procedure TdxfmPrintDialog.ConnectToPrinterDlg;
begin
  if dxConnectToNetPrinter(Self.Handle) then
  begin
    RefreshPrinterList;
    cbxPrintersChange(cbxPrinters);
    btnCancel.Caption := cxGetResourceString(@sdxBtnClose);
  end;
end;

procedure TdxfmPrintDialog.CreateControls;
begin
  seCopies.Properties.MinValue := 0;
  seCopies.Properties.MaxValue := MaxInt;
  FwpPageRanges := TdxPSWarningPane.Create(Self);
  bvlPRWarningHolder.Control := FwpPageRanges;
end;

procedure TdxfmPrintDialog.DocumentPropertiesDlg;
begin
  dxPrintDevice.Copies := seCopies.Value;
  dxPrintDevice.Collate := chbxCollate.Checked;
  if dxDocumentProperties(Handle) then
  begin
    seCopies.Value := dxPrintDevice.Copies;
    chbxCollate.Checked := dxPrintDevice.Collate;
    UpdatePrinterInfos;
    CheckModified;
    btnCancel.Caption := cxGetResourceString(@sdxBtnClose);
    dxCallNotify(OnPrintDeviceChanged, Self);
  end;
end;

procedure TdxfmPrintDialog.DrawCollatedPages(DC: HDC; const ADrawRect: TRect; ACollate: Boolean);

  procedure DrawPages(const APageRect: TRect; const AOffsets: TPoint;
    ADistance: Integer; ACollate: Boolean);

    procedure DrawPage(var R: TRect; const S: string);
    var
      Size: TSize;
    begin
      DrawEdge(DC, R, BDR_RAISEDOUTER, BF_LEFT or BF_TOP or BF_FLAT);
      DrawEdge(DC, R, BDR_RAISEDOUTER, BF_RIGHT or BF_BOTTOM);
      InflateRect(R, -ScaleFactor.Apply(1), -ScaleFactor.Apply(1));

      DrawEdge(DC, R, BDR_SUNKENINNER, BF_RIGHT or BF_BOTTOM);
      Dec(R.Right);
      Dec(R.Bottom);
      FillRect(DC, R, GetSysColorBrush(COLOR_WINDOW));

      Inc(R.Right);
      Inc(R.Bottom);
      InflateRect(R, ScaleFactor.Apply(1), ScaleFactor.Apply(1));
      GetTextExtentPoint32(DC, PChar(S), Length(S), Size);
      TextOut(DC, R.Right - Size.cX - ScaleFactor.Apply(2), R.Bottom - Size.cY - ScaleFactor.Apply(1), PChar(S), Length(S));
      with R do
        ExcludeClipRect(DC, Left, Top, Right, Bottom);
    end;

    procedure DrawPageColumn(var R: TRect; Index, Count: Integer);
    var
      I: Integer;
      S: string;
    begin
      if not ACollate then
        S := IntToStr(Index + 1);
      for I := 0 to Count - 1 do
      begin
        if ACollate then
          S := IntToStr(I + 1);
        DrawPage(R, S);
        OffsetRect(R, AOffsets.X, -AOffsets.Y);
      end;
    end;

  var
    I, C: Integer;
    R: TRect;
  begin
    C := 2 + Byte(ACollate);
    for I := 0 to 2 - Byte(ACollate) do
    begin
      R := APageRect;
      OffsetRect(R, I * ADistance, 0);
      DrawPageColumn(R, I, C);
    end;
  end;

var
  R: TRect;
  W, H, Distance, ShiftX, ShiftY: Integer;
  PageWidth, PageHeight: Integer;
  Offsets: TPoint;
begin
  PageWidth := ScaleFactor.Apply(19);
  PageHeight := ScaleFactor.Apply(24);
  Offsets := ScaleFactor.Apply(Point(10, 8));
  R := Rect(0, 0, PageWidth, PageHeight);
  W := ADrawRect.Right - ADrawRect.Left;
  H := ADrawRect.Bottom - ADrawRect.Top;
  ShiftX := ((W div (3 - Byte(ACollate))) - (PageWidth + Offsets.X * (1 + Byte(ACollate)))) div 2;
  ShiftY := ((H - PageHeight + Offsets.Y * (1 + Byte(ACollate))) div 2);
  OffsetRect(R, ShiftX, ShiftY);
  Distance := W div (3 - Byte(ACollate));
  DrawPages(R, Offsets, Distance, ACollate);
end;

function TdxfmPrintDialog.GetFileName(const S: string): string;
begin
  if S <> '' then
    if (S[1] = '"') and (S[Length(S)] = '"') then
      Result := Copy(S, 2, Length(S) - 2)
    else
      Result := ChangeFileExt(S, '.prn')
  else
    Result := '';
end;

procedure TdxfmPrintDialog.RefreshPrinterList;
begin
  with cbxPrinters.Properties do
  begin
    dxGetPrinterList(Items);
    if Items.Count > 0 then
      cbxPrinters.ItemIndex := dxPrintDevice.PrinterIndex;
  end;
end;

procedure TdxfmPrintDialog.RefreshStyleList;
var
  I: Integer;
  AStyle: TBasedxPrintStyle;
begin
  with lbxPrintStyles do
  begin
    Items.BeginUpdate;
    try
      Items.Clear;
      for I := 0 to FDialogData.StyleManager.Count - 1 do
      begin
        AStyle := FDialogData.StyleManager[I];
        Items.AddObject(AStyle.StyleCaption, AStyle);
      end;
      ItemIndex := Items.IndexOfObject(FDialogData.StyleManager.CurrentStyle);
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TdxfmPrintDialog.SavePrintDialogData;
var
  Index: Integer;
begin
  with FDialogData do
  begin
    Pages := StringReplace(edPageRanges.Text, ' ', '',  [rfReplaceAll, rfIgnoreCase]);
    FileName := GetFileName(Trim(cbxFileName.Text));
    if FileList <> nil then
    begin
      Index := FileList.IndexOf(FileName);
      if Index = -1 then
        FileList.Insert(0, FDialogData.FileName)
      else
        FileList.Exchange(0, Index);
    end;
  end;
end;

procedure TdxfmPrintDialog.SetActiveControl;
begin
  if seCopies.CanFocus then
    ActiveControl := seCopies
  else
    if cbxNumberOfPages.CanFocus then
      ActiveControl := cbxNumberOfPages
    else
      if btnOK.CanFocus then
        ActiveControl := btnOK;
end;

procedure TdxfmPrintDialog.StartSettings;
begin
  FModified := False;
  FControlsUpdating := True;
  try
    LoadStrings;
    RefreshPrinterList;

    chbxPrintToFile.Checked := FDialogData.PrintToFile;
    rbtnAllPages.Checked := FDialogData.PageRanges = prAll;
    rbtnCurrentPage.Checked := FDialogData.PageRanges = prCurrent;
    rbtnPageRanges.Checked := FDialogData.PageRanges = prRange;
    rbtnSelection.Checked := FDialogData.PageRanges = prSelection;
    edPageRanges.Text := FDialogData.Pages;
    cbxNumberOfPages.ItemIndex := Integer(FDialogData.PageNums);
    seCopies.Value := FDialogData.Copies;
    chbxCollate.Checked := FDialogData.Collate;
    if FDialogData.StyleManager <> nil then
    begin
      btnPageSetup.Caption := cxGetResourceString(@sdxBtnPrintStyles);
      btnPageSetup.Glyph.Assign(FGlyphs[not gbxPrintStyles.Visible]);
    end
    else
      if Assigned(FOnPageSetup) then
      begin
        btnPageSetup.Caption := cxGetResourceString(@sdxBtnPageSetup);
        btnPageSetup.Glyph := nil;
      end;
    cbxPrinters.Enabled := cbxPrinters.Properties.Items.Count > 0;
    btnPrinterProperties.Enabled := btnPrinterProperties.Enabled and (cbxPrinters.Properties.Items.Count > 0);
    CheckDialogFormHelpContext(Self, libtnHelp);
  finally
    UpdatePrinterInfos;
    UpdateControlsState;
    FControlsUpdating := False;
  end;
end;

procedure TdxfmPrintDialog.StyleListChanged(Sender: TObject);
begin
  if Sender = FDialogData.StyleManager then
  begin
    RefreshStyleList;
    UpdateControlsState;
  end;
end;

procedure TdxfmPrintDialog.UpdateControlsState;
begin
  cbxFileName.Enabled := chbxPrintToFile.Enabled and lichbxPrintToFile.Visible and chbxPrintToFile.Checked;
  btnBrowse.Enabled := cbxFileName.Enabled;
  btnPageSetup2.Enabled := lbxPrintStyles.ItemIndex <> -1;
  cbxPrinters.Enabled := dxPrintDevice.Printers.Count > 0; // 2.2
  if cbxPrinters.Properties.Items.Count > 0 then                      // 2.2
    cbxPrinters.ItemIndex := dxPrintDevice.PrinterIndex;   // 2.2
  btnOK.Enabled := (dxPrintDevice.Printers.Count > 0) or chbxPrintToFile.Checked;
  btnPageSetup.Enabled := Assigned(OnPageSetup) or Assigned(FDialogData.StyleManager);
end;

procedure TdxfmPrintDialog.UpdatePrinterInfos;
const
  SizeReserved = 1000;
var
  PBuffer: Pointer; {PPrinterInfo2}
  PrinterInfo2: TPrinterInfo2;
  Size: DWORD;
  PrinterHandle: THandle;
begin
  try
    if WinSpool.OpenPrinter(dxPrintDevice.CurrentDevice, PrinterHandle, nil) then
    try
      WinSpool.GetPrinter(PrinterHandle, 2, nil, 0, @Size);
      if Size > 0 then
      begin
        Inc(Size, SizeReserved);
        PBuffer := dxAllocMem(Size);
        if PBuffer <> nil then
        try
          if WinSpool.GetPrinter(PrinterHandle, 2, PBuffer, Size, @Size) then
          begin
            PrinterInfo2 := PPrinterInfo2(PBuffer)^;

            if PrinterInfo2.cJobs > 0 then
              lStatus.Caption := Format(cxGetResourceString(@sdxPrintDialogPSPrintingAndWaiting), [PrinterInfo2.cJobs])
            else
              lStatus.Caption := GetStatusString(PrinterInfo2.Status);

            if (dxPrintDevice.DeviceMode <> nil) and (PrinterInfo2.pDevMode <> nil) then
              PrinterInfo2.pDevMode^ := dxPrintDevice.DeviceMode^;

            if PrinterInfo2.Status = 0 then
              lStatus.Style.Font.Color := Font.Color
            else
              lStatus.Style.Font.Color := clHighlight;

            lType.Caption := StrPas(PrinterInfo2.pDriverName);
            lWhere.Caption := StrPas(PrinterInfo2.pPortName);
            lComment.Caption := StrPas(PrinterInfo2.pComment);
          end;
        finally
          dxFreeMem(PBuffer, Size);
        end;
      end;
    finally
      ClosePrinter(PrinterHandle);
    end;
    seCopies.Properties.MaxValue := dxPrintDevice.MaxCopies;
    seCopies.Properties.MinValue := 1;
    seCopies.Enabled := seCopies.Properties.MaxValue > seCopies.Properties.MinValue;
  except
    Application.HandleException(Self);
  end;
end;

procedure TdxfmPrintDialog.CMDialogChar(var Message: TCMDialogChar);
begin
  inherited;
  if IsAccel(Message.CharCode, gbxPrintStyles.Caption) then
  begin
    ActiveControl := lbxPrintStyles;
    Message.Result := 1;
  end;
end;

procedure TdxfmPrintDialog.cbxPrintersPropertiesDrawItem(
  AControl: TcxCustomComboBox; ACanvas: TcxCanvas; AIndex: Integer;
  const ARect: TRect; AState: TOwnerDrawState);
begin
  with TdxPrintDeviceInfo(AControl.Properties.Items.Objects[AIndex]) do
    dxDrawPrinter(ACanvas, ARect, AControl.Properties.Items[AIndex], ilPrinters, Ord(IsNetwork) + 2 * Ord(IsDefault),
      not ((odComboBoxEdit in AState) and AControl.ViewInfo.NativeStyle and IsWinVistaOrLater), ScaleFactor);
end;

procedure TdxfmPrintDialog.lbxPrintStylesDrawItem(AControl: TcxListBox;
  ACanvas: TcxCanvas; AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
begin
  dxPSDrawStyleItem(TBasedxPrintStyle(AControl.Items.Objects[AIndex]),
    AControl.InnerListBox, AIndex, AState, ARect, True, False, ScaleFactor);
end;

end.
