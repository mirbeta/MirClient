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

unit dxfmChFN;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, dxPSForm, Menus, cxClasses,
  dxCore, cxLookAndFeelPainters, cxButtons, cxControls, cxContainer, cxEdit, cxTextEdit, cxLabel, cxGroupBox,
  cxGraphics, cxLookAndFeels, dxLayoutcxEditAdapters, dxLayoutControlAdapters, dxLayoutContainer, dxLayoutLookAndFeels,
  dxLayoutControl;

type
  TfmdxChangeFileName = class(TCustomdxPSForm)
    btnCancel: TcxButton;
    btnHelp: TcxButton;
    btnOK: TcxButton;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    lblEnterNewFileName: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    edFileName: TcxTextEdit;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;

    procedure edFileNameChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    FFileName: string;
    FModified: Boolean;

    procedure LoadStrings;
    function ValidateFileName: Boolean;
  public
    function Execute: Boolean;
    property FileName: string read FFileName write FFileName;
  end;

function dxShowChooseFileNameDlg(var AFileName: string): Boolean;

implementation

{$R *.DFM}

uses
  dxPSRes, dxPSUtl;

function dxShowChooseFileNameDlg(var AFileName: string): Boolean;
begin
  with TfmdxChangeFileName.Create(nil) do
  try
    FileName := AFileName;
    Result := Execute;
    if Result then
      AFileName := FileName;
  finally
    Free;
  end;
end;

{ TfmdxChangeFileName }

procedure TfmdxChangeFileName.FormCreate(Sender: TObject);
begin
  edFileName.Properties.MaxLength := MAX_PATH;
  LoadStrings;
end;

procedure TfmdxChangeFileName.LoadStrings;
begin
  btnOK.Caption := cxGetResourceString(@sdxBtnOK);
  btnCancel.Caption := cxGetResourceString(@sdxBtnCancel);
  btnHelp.Caption := cxGetResourceString(@sdxBtnHelp);
  Caption := cxGetResourceString(@sdxENFNCaption);
  lblEnterNewFileName.Caption := cxGetResourceString(@sdxEnterNewFileName);
end;

function TfmdxChangeFileName.Execute: Boolean;
begin
  FModified := False;
  edFileName.Text := FileName;
  btnOK.Enabled := False;
  Result := (ShowModal = mrOK) and FModified;
end;

procedure TfmdxChangeFileName.edFileNameChange(Sender: TObject);
begin
  FModified := True;
  btnOK.Enabled := True;
  FileName := TEdit(Sender).Text;
end;

function TfmdxChangeFileName.ValidateFileName: Boolean;
begin
  Result := dxPSUtl.ValidateFileName(FileName);
  if not Result then
    MessageWarning(Format(cxGetResourceString(@sdxInvalidFileName), [FileName]))
  else
  begin
    Result := not FileExists(FileName);
    if not Result then
      MessageWarning(Format(cxGetResourceString(@sdxFileAlreadyExists), [FileName]));
  end;
end;

procedure TfmdxChangeFileName.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrOK then
  begin
    CanClose := ValidateFileName;
    ActiveControl := edFileName;
  end;
end;

end.
