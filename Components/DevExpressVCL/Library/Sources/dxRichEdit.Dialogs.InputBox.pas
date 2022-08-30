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

unit dxRichEdit.Dialogs.InputBox;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, dxLayoutcxEditAdapters, dxLayoutControlAdapters, Menus,
  dxLayoutContainer, StdCtrls, cxButtons, cxTextEdit, dxLayoutLookAndFeels, cxClasses, dxLayoutControl,
  dxRichEdit.Dialogs.CustomDialog,
  dxRichEdit.Dialogs.Core;

type
  TdxRichEditDialogInputBox = class(TdxRichEditCustomDialogForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    edtValue: TcxTextEdit;
    liPrompt: TdxLayoutItem;
  private
  protected
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
  public
    procedure ApplyLocalization; override;
  end;

implementation

uses
  dxCore,
  dxRichEdit.Dialogs.Strs;

{$R *.dfm}

{ TdxRichEditDialogInputBox }

procedure TdxRichEditDialogInputBox.ApplyLocalization;
begin
  btnOk.Caption := cxGetResourceString(@sdxRichEditDialogButtonOK);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
end;

function TdxRichEditDialogInputBox.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := nil;
end;

end.
