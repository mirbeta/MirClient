{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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
unit cxMaskEditTextEditor;

{$I cxVer.inc}

interface

uses
  Variants, Windows, SysUtils, Classes, Graphics, Controls, Forms,
  dxCore, cxMaskEdit, StdCtrls, cxButtons, cxControls, cxContainer, cxEdit,
  ExtCtrls, cxTextEdit, cxClasses, cxEditConsts, cxGraphics,
  cxLookAndFeels, cxLookAndFeelPainters, dxForms;

type
  TcxMaskEditTextEditorDlg = class(TdxForm)
    Label1: TLabel;
    Label2: TLabel;
    cxMaskEdit1: TcxMaskEdit;
    Label3: TLabel;
    Bevel1: TBevel;
    cxButton2: TButton;
    cxButton1: TButton;
    procedure FormShow(Sender: TObject);
    procedure cxButton1Click(Sender: TObject);
    procedure cxButton2Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    FMaskEdit: TcxCustomMaskEdit;
  public
    { Public declarations }
    property MaskEdit: TcxCustomMaskEdit read FMaskEdit write FMaskEdit;
  end;

var
  cxMaskEditTextEditorDlg: TcxMaskEditTextEditorDlg;

implementation

{$R *.dfm}

type
  TcxCustomMaskEditPropertiesAccess = class(TcxCustomMaskEditProperties);

procedure TcxMaskEditTextEditorDlg.FormShow(Sender: TObject);
var
  AProperties: TcxCustomMaskEditPropertiesAccess;
begin
  AProperties := TcxCustomMaskEditPropertiesAccess(MaskEdit.ActiveProperties);
  begin
    cxMaskEdit1.Properties.AlwaysShowBlanksAndLiterals := True;
    cxMaskEdit1.Properties.MaxLength := AProperties.MaxLength;
    cxMaskEdit1.Text := '';
    cxMaskEdit1.Properties.MaskKind := AProperties.MaskKind;
    cxMaskEdit1.Properties.EditMask := AProperties.EditMask;
    cxMaskEdit1.Properties.MaxLength := AProperties.MaxLength;
    cxMaskEdit1.Properties.CaseInsensitive := AProperties.CaseInsensitive;
    cxMaskEdit1.Text := MaskEdit.Text;
    if AProperties.IsMasked then
      Label3.Caption := AProperties.EditMask
    else
      Label3.Caption := cxGetResourceString(@scxMaskEditNoMask);
  end;
end;

procedure TcxMaskEditTextEditorDlg.cxButton1Click(Sender: TObject);
begin
  MaskEdit.Text := cxMaskEdit1.Text;
  Close;
end;

procedure TcxMaskEditTextEditorDlg.cxButton2Click(Sender: TObject);
begin
  Close;
end;

procedure TcxMaskEditTextEditorDlg.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if TranslateKey(Key) = VK_ESCAPE then
    Close
  else if TranslateKey(Key) = VK_RETURN then
  begin
    MaskEdit.Text := cxMaskEdit1.Text;
    Close;
  end;
end;

end.

