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

unit dxRichEdit.Dialogs.TabsFormController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Generics.Defaults, Generics.Collections,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.Utils.Types;

type
  { TdxTabsFormControllerParameters }

  TdxTabsFormControllerParameters = class(TdxFormControllerParameters)
  strict private
    FTabInfo: TdxTabFormattingInfo;
    FUnitConverter: TdxDocumentModelUnitConverter;
    FFormOwner: IdxFormOwner;
    FDefaultTabWidth: Integer;
  protected
    property UnitConverter: TdxDocumentModelUnitConverter read FUnitConverter;
    property FormOwner: IdxFormOwner read FFormOwner;
  public
    constructor Create(const AControl: IdxRichEditControl; ATabInfo: TdxTabFormattingInfo; ADefaultTabWidth: Integer;
      AUnitConverter: TdxDocumentModelUnitConverter; const AFormOwner: IdxFormOwner = nil);
    property DefaultTabWidth: Integer read FDefaultTabWidth write FDefaultTabWidth;
    property TabInfo: TdxTabFormattingInfo read FTabInfo;
  end;

  { TdxTabsFormController }

  TdxTabsFormController = class(TdxFormController)
  strict private
    FUnitConverter: TdxDocumentModelUnitConverter;
    FSourceTabInfo: TdxTabFormattingInfo;
    FControllerParameters: TdxTabsFormControllerParameters;
    FTabInfo: TdxTabFormattingInfo;
    FDefaultTabWidth: Integer;
    function GetSourceDefaultTabWidth: Integer; inline;
  private
    function GetFormOwner: IdxFormOwner; inline;
  public
    constructor Create(AControllerParameters: TdxTabsFormControllerParameters);
    destructor Destroy; override;
    procedure CreateCopies;
    procedure ApplyChanges; override;

    property FormOwner: IdxFormOwner read GetFormOwner;
    property SourceTabInfo: TdxTabFormattingInfo read FSourceTabInfo;
    property SourceDefaultTabWidth: Integer read GetSourceDefaultTabWidth;
    property TabFormattingInfo: TdxTabFormattingInfo read FTabInfo;
    property DefaultTabWidth: Integer read FDefaultTabWidth write FDefaultTabWidth;
    property UnitConverter: TdxDocumentModelUnitConverter read FUnitConverter;
  end;

implementation

{ TdxTabsFormControllerParameters }

constructor TdxTabsFormControllerParameters.Create(const AControl: IdxRichEditControl; ATabInfo: TdxTabFormattingInfo;
  ADefaultTabWidth: Integer; AUnitConverter: TdxDocumentModelUnitConverter; const AFormOwner: IdxFormOwner);
begin
  inherited Create(AControl);
  Assert(Assigned(ATabInfo), 'tabInfo');
  Assert(Assigned(AUnitConverter), 'unitConverter');
  FTabInfo := ATabInfo;
  FDefaultTabWidth := ADefaultTabWidth;
  FUnitConverter := AUnitConverter;
  FFormOwner := AFormOwner;
end;

{ TdxTabsFormController }

constructor TdxTabsFormController.Create(AControllerParameters: TdxTabsFormControllerParameters);
begin
  inherited Create;
  FControllerParameters := AControllerParameters;
  FUnitConverter := AControllerParameters.UnitConverter;
  FSourceTabInfo := AControllerParameters.TabInfo;
  CreateCopies;
end;

function TdxTabsFormController.GetFormOwner: IdxFormOwner;
begin
  Result := FControllerParameters.FormOwner;
end;

function TdxTabsFormController.GetSourceDefaultTabWidth: Integer;
begin
  Result := FControllerParameters.DefaultTabWidth;
end;

procedure TdxTabsFormController.CreateCopies;
begin
  FTabInfo := SourceTabInfo.Clone;
  FDefaultTabWidth := SourceDefaultTabWidth;
end;

destructor TdxTabsFormController.Destroy;
begin
  FreeAndNil(FTabInfo);
  inherited Destroy;
end;

procedure TdxTabsFormController.ApplyChanges;
begin
  SourceTabInfo.Clear;
  SourceTabInfo.AddRange(TabFormattingInfo);
  FControllerParameters.DefaultTabWidth := DefaultTabWidth;
end;

end.
