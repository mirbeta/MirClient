{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPDFViewer                                         }
{                                                                    }
{           Copyright (c) 2015-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPDFVIEWER AND ALL              }
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

unit dxPDFViewerDocumentPropertiesDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxButtons, cxClasses, dxForms,
  dxLayoutLookAndFeels, dxLayoutControlAdapters, dxLayoutContainer, dxLayoutControl, dxPDFViewer,
  dxLayoutcxEditAdapters, cxContainer, cxEdit, cxLabel, dxPDFDocument, cxGeometry, dxPDFUtils;

type
  { TdxPDFViewerDocumentPropertiesDialogForm }

  TdxPDFViewerDocumentPropertiesDialogFormClass = class of TdxPDFViewerDocumentPropertiesDialogForm;
  TdxPDFViewerDocumentPropertiesDialogForm = class(TdxForm)
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutControl1: TdxLayoutControl;
    btnOk: TcxButton;
    dxLayoutItem1: TdxLayoutItem;
    lbTitle: TcxLabel;
    liTitle: TdxLayoutItem;
    liFile: TdxLayoutItem;
    lbFile: TcxLabel;
    liLocation: TdxLayoutItem;
    lbLocation: TcxLabel;
    lbFileSize: TcxLabel;
    liFileSize: TdxLayoutItem;
    liPageCount: TdxLayoutItem;
    lbPageCount: TcxLabel;
    lbAuthor: TcxLabel;
    liAuthor: TdxLayoutItem;
    liSubject: TdxLayoutItem;
    lbSubject: TcxLabel;
    liKeywords: TdxLayoutItem;
    lbKeywords: TcxLabel;
    lgDescription: TdxLayoutGroup;
    lgAdvanced: TdxLayoutGroup;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    liProducer: TdxLayoutItem;
    lbProducer: TcxLabel;
    liVersion: TdxLayoutItem;
    lbVersion: TcxLabel;
    liPageSize: TdxLayoutItem;
    lbPageSize: TcxLabel;
    liCreated: TdxLayoutItem;
    lbCreated: TcxLabel;
    liModified: TdxLayoutItem;
    lbModified: TcxLabel;
    liApplication: TdxLayoutItem;
    lbApplication: TcxLabel;
    lgRevision: TdxLayoutGroup;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lbLocationClick(Sender: TObject);
  strict private
    FViewer: TdxPDFCustomViewer;
  protected
    procedure ApplyLocalizations; virtual;
    procedure Initialize(AViewer: TdxPDFCustomViewer); virtual;
    procedure LoadDocumentInformation; virtual;

    function GetFilePath: string;
    function GetPaperSize(const AInfo: TdxPDFPageInfo): TdxPointF;
    function GetShortPathName(AItem: TdxLayoutItem; const APath: string): string;
    function FormatFileSize(const AFileSize: Int64; AAuto: Boolean = True): string;
    function FormatPaperSize(const ASize: TdxPointF): string;

    property Viewer: TdxPDFCustomViewer read FViewer;
  public
    class function Execute(AOwner: TComponent; AViewer: TdxPDFCustomViewer): Boolean;
  end;

var
  dxPDFViewerDocumentPropertiesDialogFormClass: TdxPDFViewerDocumentPropertiesDialogFormClass = TdxPDFViewerDocumentPropertiesDialogForm;

procedure ShowDocumentPropertiesDialog(AViewer: TdxPDFCustomViewer);

implementation

uses
  IOUtils, dxCore, cxDateUtils, dxPDFTypes, dxPDFViewerDialogsStrs;

{$R *.dfm}

type
  TdxPDFViewerAccess = class(TdxPDFCustomViewer);
  TdxPDFDocumentAccess = class(TdxPDFDocument);

procedure ShowDocumentPropertiesDialog(AViewer: TdxPDFCustomViewer);
begin
  if AViewer.IsDocumentLoaded then
    dxPDFViewerDocumentPropertiesDialogFormClass.Execute(GetParentForm(AViewer), AViewer);
end;

{ TdxPDFViewerPasswordDialogForm }

class function TdxPDFViewerDocumentPropertiesDialogForm.Execute(AOwner: TComponent; AViewer: TdxPDFCustomViewer): Boolean;
var
  ADialog: TdxPDFViewerDocumentPropertiesDialogForm;
begin
  ADialog := dxPDFViewerDocumentPropertiesDialogFormClass.Create(AOwner);
  try
    ADialog.Initialize(AViewer);
    Result := ADialog.ShowModal = mrOk;
  finally
    ADialog.Free;
  end;
end;

procedure TdxPDFViewerDocumentPropertiesDialogForm.ApplyLocalizations;
begin
  Caption := cxGetResourceString(@sdxPDFViewerDocumentPropertiesDialogCaption);
  btnOk.Caption := cxGetResourceString(@sdxPDFViewerPasswordDialogButtonOK);

  lgDescription.Caption := cxGetResourceString(@sdxPDFViewerDocumentPropertiesDialogDescription);
  liFile.Caption := cxGetResourceString(@sdxPDFViewerDocumentPropertiesDialogFile);
  liTitle.Caption := cxGetResourceString(@sdxPDFViewerDocumentPropertiesDialogTitle);
  liAuthor.Caption := cxGetResourceString(@sdxPDFViewerDocumentPropertiesDialogAuthor);
  liSubject.Caption := cxGetResourceString(@sdxPDFViewerDocumentPropertiesDialogSubject);
  liKeywords.Caption := cxGetResourceString(@sdxPDFViewerDocumentPropertiesDialogKeywords);

  lgRevision.Caption := cxGetResourceString(@sdxPDFViewerDocumentPropertiesDialogRevision);
  liCreated.Caption := cxGetResourceString(@sdxPDFViewerDocumentPropertiesDialogCreated);
  liModified.Caption := cxGetResourceString(@sdxPDFViewerDocumentPropertiesDialogModified);
  liApplication.Caption := cxGetResourceString(@sdxPDFViewerDocumentPropertiesDialogApplication);

  lgAdvanced.Caption := cxGetResourceString(@sdxPDFViewerDocumentPropertiesDialogAdvanced);
  liProducer.Caption := cxGetResourceString(@sdxPDFViewerDocumentPropertiesDialogProducer);
  liVersion.Caption := cxGetResourceString(@sdxPDFViewerDocumentPropertiesDialogVersion);
  liLocation.Caption := cxGetResourceString(@sdxPDFViewerDocumentPropertiesDialogLocation);
  liFileSize.Caption := cxGetResourceString(@sdxPDFViewerDocumentPropertiesDialogFileSize);
  liPageCount.Caption := cxGetResourceString(@sdxPDFViewerDocumentPropertiesDialogNumberOfPages);
  liPageSize.Caption := cxGetResourceString(@sdxPDFViewerDocumentPropertiesDialogPageSize);
end;

procedure TdxPDFViewerDocumentPropertiesDialogForm.Initialize(AViewer: TdxPDFCustomViewer);
begin
  FViewer := AViewer;
  SetControlLookAndFeel(Self, TdxPDFViewerAccess(FViewer).DialogsLookAndFeel);
  ApplyLocalizations;
  LoadDocumentInformation;
end;

procedure TdxPDFViewerDocumentPropertiesDialogForm.lbLocationClick(Sender: TObject);
begin
  dxShellExecute(GetFilePath);
end;

procedure TdxPDFViewerDocumentPropertiesDialogForm.LoadDocumentInformation;
const
  VersionNameMap: array[TdxPDFVersion] of string = ('1.0', '1.1', '1.2', '1.3', '1.4', '1.5', '1.6', '1.7');
begin
  lbFile.Caption := TPath.GetFileName(Viewer.Document.Information.FileName);
  lbTitle.Caption := Viewer.Document.Information.Title;
  lbAuthor.Caption := Viewer.Document.Information.Author;
  lbSubject.Caption := Viewer.Document.Information.Subject;
  lbKeywords.Caption := Viewer.Document.Information.Keywords;

  lbCreated.Caption := cxDateTimeToText(Viewer.Document.Information.CreationDate);
  lbModified.Caption := cxDateTimeToText(Viewer.Document.Information.ModificationDate);
  lbApplication.Caption := Viewer.Document.Information.Application;

  lbProducer.Caption := Viewer.Document.Information.Producer;
  lbVersion.Caption := VersionNameMap[Viewer.Document.Information.Version];

  lbLocation.Caption := GetFilePath;
  if lbLocation.Caption <> '' then
  begin
    if lbLocation.Caption[Length(lbLocation.Caption)] <> '\' then
      lbLocation.Caption := lbLocation.Caption + '\';
    lbLocation.Caption := GetShortPathName(liLocation, lbLocation.Caption);
  end;

  lbFileSize.Caption := FormatFileSize(Viewer.Document.Information.FileSize) + ' (' +
    FormatFileSize(Viewer.Document.Information.FileSize, False) + ')';
  lbPageCount.Caption := IntToStr(Viewer.PageCount);
  lbPageSize.Caption := FormatPaperSize(GetPaperSize(Viewer.Document.PageInfo[Viewer.CurrentPageIndex]));
end;

function TdxPDFViewerDocumentPropertiesDialogForm.GetFilePath: string;
begin
  Result := Viewer.Document.Information.FileName;
  if Result <> '' then
    Result := TPath.GetDirectoryName(TPath.GetFullPath(Result))
end;

function TdxPDFViewerDocumentPropertiesDialogForm.GetPaperSize(const AInfo: TdxPDFPageInfo): TdxPointF;
begin
  Result.X := AInfo.Size.X * AInfo.UserUnit / 72;
  Result.Y := AInfo.Size.Y * AInfo.UserUnit / 72;
end;

function TdxPDFViewerDocumentPropertiesDialogForm.GetShortPathName(AItem: TdxLayoutItem; const APath: string): string;
begin
  Result := cxGetStringAdjustedToWidth(AItem.ViewInfo.CaptionViewInfo.Font, APath, AItem.ViewInfo.ControlViewInfo.Width);
end;

function TdxPDFViewerDocumentPropertiesDialogForm.FormatFileSize(const AFileSize: Int64; AAuto: Boolean = True): string;
begin
  Result := TdxPDFUtils.FormatFileSize(AFileSize, AAuto);
end;

function TdxPDFViewerDocumentPropertiesDialogForm.FormatPaperSize(const ASize: TdxPointF): string;
const
  PageSizeFormatTemplate = '##0.#';
begin
  Result := Format('%s x %s %s', [
    FormatFloat(PageSizeFormatTemplate, ASize.X),
    FormatFloat(PageSizeFormatTemplate, ASize.Y),
    cxGetResourceString(@sdxPDFViewerUnitsInches)]);
end;

procedure TdxPDFViewerDocumentPropertiesDialogForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if TranslateKey(Key) = VK_ESCAPE then
    Close
end;

end.
