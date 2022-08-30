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

unit dxRichEdit.InnerControl.Mouse;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxCore,
  dxRichEdit.Utils.Mouse,
  dxRichEdit.Control.HitTest,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.PieceTable;

type
  { TdxRichEditCustomMouseController }

  TdxRichEditCustomMouseController = class abstract(TdxCustomMouseController)
  strict private
    FTableViewInfo: TdxTableViewInfo;
    procedure SetTableViewInfo(const Value: TdxTableViewInfo);
    class function IsInlinePictureInsideFloatingObject(AHitTestResult: TdxRichEditHitTestResult): Boolean; static;
  protected
    procedure ControlBeginUpdate; virtual; abstract;
    procedure ControlEndUpdate; virtual; abstract;
  public
    function CreateRectangularObjectResizeState(const AHotZone: IdxHotZone; AResult: TdxRichEditHitTestResult): IdxCustomMouseState; virtual; abstract;
    function CreateRectangularObjectRotateState(const AHotZone: IdxHotZone; AResult: TdxRichEditHitTestResult): IdxCustomMouseState; virtual; abstract;
    procedure ChangeActivePieceTable(APieceTable: TdxPieceTable; AHitTestResult: TdxRichEditHitTestResult = nil); virtual; abstract;
    function DeactivateTextBoxPieceTableIfNeed(APieceTable: TdxPieceTable;
      AHitTestResult: TdxRichEditHitTestResult): Boolean;

    class function IsInlinePictureBoxHit(AHitTestResult: TdxRichEditHitTestResult): Boolean; static;

    property TableViewInfo: TdxTableViewInfo read FTableViewInfo write SetTableViewInfo;
  end;

implementation

uses
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.FloatingObjectRange;

{ TdxRichEditCustomMouseController }

function TdxRichEditCustomMouseController.DeactivateTextBoxPieceTableIfNeed(APieceTable: TdxPieceTable;
  AHitTestResult: TdxRichEditHitTestResult): Boolean;
var
  ATextBoxPieceTable: TdxTextBoxContentType;
begin
  ATextBoxPieceTable := Safe<TdxTextBoxContentType>.Cast(APieceTable.ContentType);
  if ATextBoxPieceTable <> nil then
  begin
    if IsInlinePictureBoxHit(AHitTestResult) then
      Exit(False);

    ControlBeginUpdate;
    try
      ChangeActivePieceTable(TdxPieceTable(ATextBoxPieceTable.AnchorRun.PieceTable), AHitTestResult);
      ATextBoxPieceTable.AnchorRun.Select;
    finally
      ControlEndUpdate;
    end;
    Exit(True);
  end;
  Result := False;
end;

class function TdxRichEditCustomMouseController.IsInlinePictureBoxHit(AHitTestResult: TdxRichEditHitTestResult): Boolean;
var
  AInlinePictureBox: TdxInlinePictureBox;
  AAnchorRun: TdxFloatingObjectAnchorRun;
begin
  if AHitTestResult.Character <> nil then
  begin
    AInlinePictureBox := Safe<TdxInlinePictureBox>.Cast(AHitTestResult.Box);
    if AInlinePictureBox = nil then
      Exit(False);
    if AHitTestResult.FloatingObjectBox = nil then
      Exit(True);
    AAnchorRun := AHitTestResult.FloatingObjectBox.GetFloatingObjectRun;
    if IsInlinePictureInsideFloatingObject(AHitTestResult) then
      Exit(True);
    Exit(AAnchorRun.FloatingObjectProperties.IsBehindDoc);
  end;
  Result := False;
end;

procedure TdxRichEditCustomMouseController.SetTableViewInfo(const Value: TdxTableViewInfo);
begin
  if FTableViewInfo <> Value then
  begin
    TdxTableViewInfo.Release(FTableViewInfo);
    FTableViewInfo := Value;
    TdxTableViewInfo.AddReference(FTableViewInfo);
  end;
end;

class function TdxRichEditCustomMouseController.IsInlinePictureInsideFloatingObject(AHitTestResult: TdxRichEditHitTestResult): Boolean;
var
  APage: TdxPage;
begin
  if AHitTestResult.FloatingObjectBox.DocumentLayout = nil then
    Exit(False);
  APage := AHitTestResult.FloatingObjectBox.DocumentLayout.Pages.First;
  Result := AHitTestResult.Page = APage;
end;

end.
