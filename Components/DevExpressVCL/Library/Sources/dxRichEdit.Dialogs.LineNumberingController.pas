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

unit dxRichEdit.Dialogs.LineNumberingController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Classes, SysUtils, Variants, Types,
  Generics.Defaults, Generics.Collections,
  dxRichEdit.Types,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.View.Core,
  dxRichEdit.Utils.Types,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.DocumentModel.SectionFormatting;

type

  { TdxLineNumberingFormControllerParameters }

  TdxLineNumberingFormControllerParameters = class(TdxFormControllerParameters)
  strict private
    FLineNumberingInfo: TdxLineNumberingInfo;
  public
    constructor Create(const AControl: IdxRichEditControl; ALineNumberingInfo: TdxLineNumberingInfo);
    property LineNumberingInfo: TdxLineNumberingInfo read FLineNumberingInfo;
  end;

  { TdxLineNumberingFormController }

  TdxLineNumberingFormController = class(TdxFormController)
  strict private
    FSourceLineNumberingInfo: TdxLineNumberingInfo;
    FLineNumberingInfo: TdxLineNumberingInfo;
    function GetDistance: Integer;
    procedure SetDistance(const AValue: Integer);
    function GetStartingLineNumber: Integer;
    procedure SetStartingLineNumber(const AValue: Integer);
    function GetStep: Integer;
    procedure SetStep(const AValue: Integer);
    function GetNumberingRestartType: TdxLineNumberingRestart;
    procedure SetNumberingRestartType(const AValue: TdxLineNumberingRestart);
  public
    constructor Create(AControllerParameters: TdxLineNumberingFormControllerParameters);
    destructor Destroy; override;
    procedure ApplyChanges; override;

    property SourceLineNumberingInfo: TdxLineNumberingInfo read FSourceLineNumberingInfo;
    property Distance: Integer read GetDistance write SetDistance;
    property StartingLineNumber: Integer read GetStartingLineNumber write SetStartingLineNumber;
    property Step: Integer read GetStep write SetStep;
    property NumberingRestartType: TdxLineNumberingRestart read GetNumberingRestartType write SetNumberingRestartType;
  end;

implementation

{ TdxLineNumberingFormControllerParameters }

constructor TdxLineNumberingFormControllerParameters.Create(const AControl: IdxRichEditControl; ALineNumberingInfo: TdxLineNumberingInfo);
begin
  inherited Create(AControl);
  Assert(ALineNumberingInfo <> nil);
  FLineNumberingInfo := ALineNumberingInfo;
end;

{ TdxLineNumberingFormController }

constructor TdxLineNumberingFormController.Create(AControllerParameters: TdxLineNumberingFormControllerParameters);
begin
  inherited Create;
  FSourceLineNumberingInfo := AControllerParameters.LineNumberingInfo;
  FLineNumberingInfo := FSourceLineNumberingInfo.Clone;
end;

destructor TdxLineNumberingFormController.Destroy;
begin
  FLineNumberingInfo.Free;
  inherited Destroy;
end;

function TdxLineNumberingFormController.GetDistance: Integer;
begin
  Result := FLineNumberingInfo.Distance;
end;

procedure TdxLineNumberingFormController.SetDistance(const AValue: Integer);
begin
  FLineNumberingInfo.Distance := AValue;
end;

function TdxLineNumberingFormController.GetStartingLineNumber: Integer;
begin
  Result := FLineNumberingInfo.StartingLineNumber;
end;

procedure TdxLineNumberingFormController.SetStartingLineNumber(const AValue: Integer);
begin
  FLineNumberingInfo.StartingLineNumber := AValue;
end;

function TdxLineNumberingFormController.GetStep: Integer;
begin
  Result := FLineNumberingInfo.Step;
end;

procedure TdxLineNumberingFormController.SetStep(const AValue: Integer);
begin
  FLineNumberingInfo.Step := AValue;
end;

function TdxLineNumberingFormController.GetNumberingRestartType: TdxLineNumberingRestart;
begin
  Result := FLineNumberingInfo.NumberingRestartType;
end;

procedure TdxLineNumberingFormController.SetNumberingRestartType(const AValue: TdxLineNumberingRestart);
begin
  FLineNumberingInfo.NumberingRestartType := AValue;
end;

procedure TdxLineNumberingFormController.ApplyChanges;
begin
  FSourceLineNumberingInfo.CopyFrom(FLineNumberingInfo);
end;

end.
