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

unit dxPSfmReportProperties;

interface

{$I cxVer.inc}

uses
  Types, Windows, Classes, Controls, Graphics, StdCtrls, ComCtrls, ExtCtrls, Forms, dxPSCore, dxPSForm, dxExtCtrls,
  Menus, cxLookAndFeelPainters, cxButtons, cxControls, cxContainer, cxEdit, cxLabel, cxTextEdit, cxMemo, cxPC,
  cxGeometry, dxCore, cxGraphics, cxLookAndFeels, dxLayoutLookAndFeels, cxClasses, dxLayoutContainer,
  dxLayoutControl, dxLayoutcxEditAdapters, dxLayoutControlAdapters;

type

  { TdxfmPSReportProperties }

  TdxfmPSReportProperties = class(TCustomdxPSExplorerItemPropertySheets)
    btnCancel: TcxButton;
    btnHelp: TcxButton;
    btnOK: TcxButton;
    btnPreview: TcxButton;
    bvlPreviewHost: TdxLayoutItem;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup6: TdxLayoutGroup;
    dxLayoutGroup8: TdxLayoutGroup;
    dxLayoutGroup9: TdxLayoutGroup;
    dxLayoutImageItem1: TdxLayoutImageItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    dxLayoutSeparatorItem2: TdxLayoutSeparatorItem;
    edCreationDate: TcxTextEdit;
    edCreator: TcxTextEdit;
    edName: TcxTextEdit;
    lblCreationDate: TdxLayoutItem;
    lblCreator: TdxLayoutItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    libtnHelp: TdxLayoutItem;
    memDescription: TcxMemo;
    tshDescription: TdxLayoutGroup;
    tshPreview: TdxLayoutGroup;
    tshSummary: TdxLayoutGroup;

    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure PreviewClick(Sender: TObject);
  private
    FPreviewBox: TdxPSImageScrollBox;

    function GetFormCaption: string;
    function GetReportCaption: string;
    function GetReportCreationDateTime: TDateTime;
    function GetReportCreator: string;
    function GetReportDescription: string;
    procedure SetReportCaption(const Value: string);
    procedure SetReportCreationDateTime(const Value: TDateTime);
    procedure SetReportCreator(const Value: string);
    procedure SetReportDescription(const Value: string);
    procedure SetReportPreview(Value: TGraphic);
  protected
    procedure CreateWnd; override;

    procedure BeforeConstruction; override;
    procedure Done; override;
    procedure Initialize; override;

    function HasGraphic: Boolean;
    procedure CreateControls; virtual;
    procedure LoadStrings; virtual;
    procedure PrepareControlColors;
  public
    constructor Create(AOwner: TComponent); override;

    function ExplorerItem: TdxPSExplorerItem; reintroduce; overload;

    property FormCaption: string read GetFormCaption;
    property PreviewBox: TdxPSImageScrollBox read FPreviewBox;
    property ReportCaption: string read GetReportCaption write SetReportCaption;
    property ReportCreationDateTime: TDateTime read GetReportCreationDateTime write SetReportCreationDateTime;
    property ReportCreator: string read GetReportCreator write SetReportCreator;
    property ReportDescription: string read GetReportDescription write SetReportDescription;
    property ReportPreview: TGraphic write SetReportPreview;
  end;

implementation

{$R *.dfm}

uses
  Themes, Messages, SysUtils, Dialogs, ExtDlgs, cxFormats, dxPSGlbl, dxPSUtl, dxPSRes, dxPcPrVW;

{ TdxfmPSReportProperties }

constructor TdxfmPSReportProperties.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HelpContext := dxPSGlbl.dxhcPSReportPropertiesDlg;
  CheckDialogFormHelpContext(Self, libtnHelp);
  CreateControls;
  LoadStrings;
end;

function TdxfmPSReportProperties.ExplorerItem: TdxPSExplorerItem;
begin
  Result := inherited ExplorerItem as TdxPSExplorerItem;
end;

procedure TdxfmPSReportProperties.CreateWnd;
begin
  inherited CreateWnd;
  SendMessage(Handle, WM_SETICON, 1, Icon.Handle);
end;

procedure TdxfmPSReportProperties.BeforeConstruction;
begin
  inherited BeforeConstruction;
  Options := Options + [foSizeableDialog];
end;

procedure TdxfmPSReportProperties.Done;
begin
  if ModalResult = mrOK then
    with ExplorerItem.ReportDocument do
    begin
      BeginUpdate;
      try
        Caption := ReportCaption;
        Description := ReportDescription;
      finally
        EndUpdate;
      end;
    end;
end;

procedure TdxfmPSReportProperties.Initialize;
begin
  ReportCaption := ExplorerItem.ReportDocument.Caption;
  ReportCreator := ExplorerItem.ReportDocument.Creator;
  ReportCreationDateTime := ExplorerItem.ReportDocument.CreationDate;
  ReportDescription := ExplorerItem.ReportDocument.Description;
  ReportPreview := ExplorerItem.ReportDocument.Preview;

  edName.Properties.ReadOnly := ExplorerItem.IsCurrentlyLoaded;
  Caption := FormCaption;
  PrepareControlColors;

  btnPreview.Enabled := HasGraphic;
  PreviewBox.Enabled := HasGraphic;
end;

procedure TdxfmPSReportProperties.CreateControls;
begin
  FPreviewBox := TdxPSImageScrollBox.Create(Self);
  bvlPreviewHost.Control := FPreviewBox;
end;

function TdxfmPSReportProperties.HasGraphic: Boolean;
begin
  Result := PreviewBox.HasGraphic;
end;

procedure TdxfmPSReportProperties.LoadStrings;
begin
  btnOK.Caption := cxGetResourceString(@sdxBtnOK);
  btnCancel.Caption := cxGetResourceString(@sdxBtnCancel);
  btnHelp.Caption := cxGetResourceString(@sdxBtnHelp);
  btnPreview.Caption := AddEndEllipsis(cxGetResourceString(@sdxPreview));

  tshSummary.Caption := cxGetResourceString(@sdxSummary);
  lblCreator.Caption := cxGetResourceString(@sdxCreator);
  lblCreationDate.Caption  := cxGetResourceString(@sdxCreationDate);
  tshDescription.Caption := DropColon(DropAmpersand(cxGetResourceString(@sdxDescription)));
  tshPreview.Caption := DropAmpersand(cxGetResourceString(@sdxPreview));
end;

function TdxfmPSReportProperties.GetFormCaption: string;
begin
  if ReportCaption = '' then
    Result := '"' + ReportCaption + '"  '
  else
    Result := '';

  Result := Result + DropAmpersand(cxGetResourceString(@sdxProperties));
end;

function TdxfmPSReportProperties.GetReportCaption: string;
begin
  Result := edName.Text;
end;

function TdxfmPSReportProperties.GetReportCreationDateTime: TDateTime;
begin
  Result := SysUtils.StrToDateTime(edCreationDate.Text);
end;

function TdxfmPSReportProperties.GetReportCreator: string;
begin
  Result := edCreator.Text;
end;

function TdxfmPSReportProperties.GetReportDescription: string;
begin
  Result := memDescription.Text;
end;

procedure TdxfmPSReportProperties.SetReportCaption(const Value: string);
begin
  edName.Text := Value;
end;

procedure TdxfmPSReportProperties.SetReportCreationDateTime(const Value: TDateTime);
const
  LongDateFormat = 'mmmm d, yyyy';
var
  SystemTime: TSystemTime;
  DefaultLCID: LCID;
  DateFormat, TimeFormat: string;
  Buffer: array[Byte] of Char;
begin
  DateTimeToSystemTime(Value, SystemTime);
  DefaultLCID := GetThreadLocale;

  DateFormat := GetLocaleStr(DefaultLCID, LOCALE_SLONGDATE, LongDateFormat);
  GetDateFormat(DefaultLCID, 0, @SystemTime, PChar(DateFormat), @Buffer, SizeOf(Buffer));
  edCreationDate.Text := Buffer;

  TimeFormat := GetLocaleStr(DefaultLCID, LOCALE_STIMEFORMAT, dxFormatSettings.LongTimeFormat);
  GetTimeFormat(DefaultLCID, 0, @SystemTime, PChar(TimeFormat), @Buffer, SizeOf(Buffer));
  edCreationDate.Text := edCreationDate.Text + ',   ' + Buffer;
end;

procedure TdxfmPSReportProperties.SetReportCreator(const Value: string);
begin
  edCreator.Text := Value;
end;

procedure TdxfmPSReportProperties.SetReportDescription(const Value: string);
begin
  memDescription.Text := Value;
end;

procedure TdxfmPSReportProperties.SetReportPreview(Value: TGraphic);
begin
  PreviewBox.Picture.Graphic := Value;
end;

procedure TdxfmPSReportProperties.PrepareControlColors;
const
  ColorsMap: array[Boolean] of TColor = (clBtnFace, clWindow);
var
  ABkgColor: TColor;
begin
  ABkgColor := Painter.DefaultEditorBackgroundColor(edName.Properties.ReadOnly);
  if ABkgColor = clDefault then
    ABkgColor := ColorsMap[edName.Properties.ReadOnly];
  edName.Style.Color := ABkgColor;
end;

procedure TdxfmPSReportProperties.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := (ModalResult = mrCancel) or ExplorerItem.IsCurrentlyLoaded or
    not ExplorerItem.IsNameChanged(ReportCaption) or ExplorerItem.CanRenameTo(ReportCaption);
  if not CanClose then
  begin
    dxPSUtl.MessageError(ExplorerItem.CannotRenameMessageText(ExplorerItem.DisplayName, ReportCaption));
    ActiveControl := edName;
    edName.SelectAll;
  end;
end;

procedure TdxfmPSReportProperties.PreviewClick(Sender: TObject);
begin
  dxPcPrVW.dxShowPicturePreview(PreviewBox.Picture.Graphic);
end;

end.
