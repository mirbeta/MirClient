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

unit dxPDFViewerPasswordDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxClasses, dxLayoutContainer,
  dxLayoutControl, dxLayoutLookAndFeels, dxLayoutControlAdapters, dxLayoutcxEditAdapters, cxContainer, cxEdit,
  cxTextEdit, cxButtons, dxForms, dxPDFViewer;

type
  { TdxPDFViewerPasswordDialogForm }

  TdxPDFViewerPasswordDialogFormClass = class of TdxPDFViewerPasswordDialogForm;
  TdxPDFViewerPasswordDialogForm = class(TdxForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    edPassword: TcxTextEdit;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    liPassword: TdxLayoutItem;
    liDocumentIsProtected: TdxLayoutLabeledItem;
  strict private
    FLookAndFeel: TcxLookAndFeel;
  protected
    procedure ApplyLocalizations; virtual;
    procedure Initialize(ALookAndFeel: TcxLookAndFeel); virtual;
  public
    class function Execute(AOwner: TComponent; ALookAndFeel: TcxLookAndFeel; out APassword: string): Boolean;
  end;

var
  dxPDFViewerPasswordDialogClass: TdxPDFViewerPasswordDialogFormClass = TdxPDFViewerPasswordDialogForm;

function ShowPasswordDialog(AViewer: TdxPDFCustomViewer; out APassword: string): Boolean;

implementation

uses
  dxCore, dxPDFViewerDialogsStrs;

{$R *.dfm}

type
  TdxPDFCustomViewerAccess = class(TdxPDFCustomViewer);

function ShowPasswordDialog(AViewer: TdxPDFCustomViewer; out APassword: string): Boolean;
begin
  Result := TdxPDFViewerPasswordDialogForm.Execute(GetParentForm(AViewer),
    TdxPDFCustomViewerAccess(AViewer).DialogsLookAndFeel, APassword);
end;

{ TdxPDFViewerPasswordDialogForm }

class function TdxPDFViewerPasswordDialogForm.Execute(AOwner: TComponent; ALookAndFeel: TcxLookAndFeel;
  out APassword: string): Boolean;
var
  ADialog: TdxPDFViewerPasswordDialogForm;
begin
  ADialog := dxPDFViewerPasswordDialogClass.Create(AOwner);
  try
    ADialog.Initialize(ALookAndFeel);
    Result := ADialog.ShowModal = mrOk;
    if Result then
      APassword := ADialog.edPassword.Text;
  finally
    ADialog.Free;
  end;
end;

procedure TdxPDFViewerPasswordDialogForm.ApplyLocalizations;
begin
  Caption := cxGetResourceString(@sdxPDFViewerPasswordDialogCaption);
  liDocumentIsProtected.Caption := cxGetResourceString(@sdxPDFViewerPasswordDialogProtectedDocument);
  liPassword.Caption := cxGetResourceString(@sdxPDFViewerPasswordDialogPassword);
  btnCancel.Caption := cxGetResourceString(@sdxPDFViewerPasswordDialogButtonCancel);
  btnOk.Caption := cxGetResourceString(@sdxPDFViewerPasswordDialogButtonOK);
end;

procedure TdxPDFViewerPasswordDialogForm.Initialize(ALookAndFeel: TcxLookAndFeel);
begin
  FLookAndFeel := ALookAndFeel;
  SetControlLookAndFeel(Self, FLookAndFeel);
  ApplyLocalizations;
end;

end.
