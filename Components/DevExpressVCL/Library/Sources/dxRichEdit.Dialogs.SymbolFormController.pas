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

unit dxRichEdit.Dialogs.SymbolFormController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils,
  dxRichEdit.Commands.Numbering,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Dialogs.CustomDialog, dxRichEdit.Utils.Properties;

type

  { TdxInsertSymbolControllerParameters }

  TdxInsertSymbolControllerParameters = class(TdxFormControllerParameters)
  private
    FCallbackData: TObject;
    FFontName: string;
    FUnicodeChar: Char;
    FOnApplyChanges: TdxShowSymbolFormCallback;
  public
    property CallbackData: TObject read FCallbackData;
    property FontName: string read FFontName;
    property UnicodeChar: Char read FUnicodeChar;
    property OnApplyChanges: TdxShowSymbolFormCallback read FOnApplyChanges;
  end;

  { TdxRichEditInsertSymbolControllerParameters }

  TdxRichEditInsertSymbolControllerParameters = class(TdxInsertSymbolControllerParameters)
  public
    constructor Create(const AControl: IdxRichEditControl; const ASymbolProperties: TdxSymbolProperties;
      const ACallback: TdxShowSymbolFormCallback; ACallbackData: TObject);
  end;

  { TdxRichEditInsertSymbolController }

  TdxRichEditInsertSymbolController = class(TdxFormController)
  private
    FCallbackData: TObject;
    FControl: IdxRichEditControl;
    FFontName: string;
    FUnicodeChar: Char;
    FOnApplyChanges: TdxShowSymbolFormCallback;
  public
    constructor Create(const AControllerParameters: TdxRichEditInsertSymbolControllerParameters);
    procedure ApplyChanges; override;

    property Control: IdxRichEditControl read FControl;
    property FontName: string read FFontName write FFontName;
    property UnicodeChar: Char read FUnicodeChar write FUnicodeChar;
  end;

implementation

uses
  dxRichEdit.Commands.Insert,
  dxRichEdit.Commands.ChangeProperties;

{ TdxRichEditInsertSymbolController }

constructor TdxRichEditInsertSymbolController.Create(
  const AControllerParameters: TdxRichEditInsertSymbolControllerParameters);
begin
  inherited Create;
  FCallbackData := AControllerParameters.CallbackData;
  FControl := AControllerParameters.Control;
  FFontName := AControllerParameters.FontName;
  FUnicodeChar := AControllerParameters.UnicodeChar;
  FOnApplyChanges := AControllerParameters.FOnApplyChanges;
end;

procedure TdxRichEditInsertSymbolController.ApplyChanges;
var
  ACommand: TdxInsertSymbolCommand;
  AState: TdxInsertSymbolCommandUIState;
begin
  if Assigned(FOnApplyChanges) then
  begin
    FOnApplyChanges(TdxSymbolProperties.Create(UnicodeChar, FontName), FCallbackData);
    Exit;
  end;

  ACommand := TdxInsertSymbolCommand.Create(Control);
  try
    if ACommand.CanExecute then
    begin
      AState := ACommand.CreateDefaultCommandUIState as TdxInsertSymbolCommandUIState;
      AState.Value := TdxSymbolProperties.Create(UnicodeChar, FontName);
      ACommand.ForceExecute(AState);
    end;
  finally
    ACommand.Free;
  end;
end;

{ TdxRichEditInsertSymbolControllerParameters }

constructor TdxRichEditInsertSymbolControllerParameters.Create(const AControl: IdxRichEditControl;
  const ASymbolProperties: TdxSymbolProperties; const ACallback: TdxShowSymbolFormCallback; ACallbackData: TObject);
begin
  inherited Create(AControl);
  FFontName := ASymbolProperties.FontName;
  FUnicodeChar := ASymbolProperties.UnicodeChar;
  FOnApplyChanges := ACallback;
end;

end.
