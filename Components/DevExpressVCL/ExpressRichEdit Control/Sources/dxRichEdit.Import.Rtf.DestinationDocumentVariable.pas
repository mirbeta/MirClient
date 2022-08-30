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

unit dxRichEdit.Import.Rtf.DestinationDocumentVariable;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, dxCore,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Import.Rtf,
  dxRichEdit.Import.Rtf.DestinationPieceTable;

type
  { TdxDocumentVariableDestination }

  TdxDocumentVariableDestination = class(TdxStringValueDestination)
  strict private
    FName: string;
    FValue: string;
    FIsNameRead: Boolean;
  protected
    function CreateEmptyClone: TdxStringValueDestination; override;
  public
    constructor Create(ARtfImporter: TdxRtfImporter); override;
    procedure AfterPopRtfState; override;
    procedure NestedGroupFinished(ANestedDestination: TdxRichEditRtfDestinationBase); override;
  end;

implementation

uses
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.DocumentProperties;

{ TdxDocumentVariableDestination }

constructor TdxDocumentVariableDestination.Create(ARtfImporter: TdxRtfImporter);
begin
  inherited Create(ARtfImporter);
  FIsNameRead := True;
end;

function TdxDocumentVariableDestination.CreateEmptyClone: TdxStringValueDestination;
begin
  Result := TdxDocumentVariableDestination.Create(Importer);
end;

procedure TdxDocumentVariableDestination.AfterPopRtfState;
begin
  inherited AfterPopRtfState;
  if (FName <> '') and (FValue <> '') then
  begin
    if FName <> TdxDocumentProperties.UpdateDocVariableFieldsBeforePrintDocVarName then
      Importer.DocumentModel.Variables.Add(FName, FValue)
    else
      Importer.DocumentModel.DocumentProperties.SetUpdateFieldsBeforePrintFromDocVar(FValue);
  end;
end;

procedure TdxDocumentVariableDestination.NestedGroupFinished(ANestedDestination: TdxRichEditRtfDestinationBase);
var
  ANested: TdxDocumentVariableDestination;
  ANestedValue: string;
begin
  inherited NestedGroupFinished(ANestedDestination);
  ANested := Safe<TdxDocumentVariableDestination>.Cast(ANestedDestination);
  if ANested = nil then
    Exit;
  ANestedValue := Trim(ANested.Value);
  if FIsNameRead then
  begin
    FName := ANestedValue;
    FIsNameRead := False;
  end
  else
    FValue := ANestedValue;
end;

end.
