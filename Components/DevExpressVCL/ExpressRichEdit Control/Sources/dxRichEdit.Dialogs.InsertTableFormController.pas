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

unit dxRichEdit.Dialogs.InsertTableFormController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface
uses
  SysUtils, Generics.Defaults, Generics.Collections,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.Utils.Properties,
  dxRichEdit.Utils.Types;

type

  { TdxInsertTableFormControllerParameters }

  TdxInsertTableFormControllerParameters = class(TdxFormControllerParameters)
  strict private
    FParameters: TdxCreateTableParameters;
  public
    constructor Create(const AControl: IdxRichEditControl; const AParameters: TdxCreateTableParameters);
    property Parameters: TdxCreateTableParameters read FParameters;
  end;

  { TdxInsertTableFormController }

  TdxInsertTableFormController = class(TdxFormController)
  strict private
    FSourceParameters: TdxCreateTableParameters;
    FColumnCount: Integer;
    FRowCount: Integer;
  protected
    procedure InitializeController; virtual;
  public
    constructor Create(AControllerParameters: TdxInsertTableFormControllerParameters);
    procedure ApplyChanges; override;

    property SourceParameters: TdxCreateTableParameters read FSourceParameters;
    property ColumnCount: Integer read FColumnCount write FColumnCount;
    property RowCount: Integer read FRowCount write FRowCount;
  end;

implementation

{ TdxInsertTableFormControllerParameters }

constructor TdxInsertTableFormControllerParameters.Create(const AControl: IdxRichEditControl; const AParameters: TdxCreateTableParameters);
begin
  inherited Create(AControl);
  FParameters := AParameters;
end;

{ TdxInsertTableFormController }

constructor TdxInsertTableFormController.Create(AControllerParameters: TdxInsertTableFormControllerParameters);
begin
  inherited Create;
  Assert(AControllerParameters <> nil);
  FSourceParameters := AControllerParameters.Parameters;
  InitializeController;
end;

procedure TdxInsertTableFormController.InitializeController;
begin
  ColumnCount := SourceParameters.ColumnCount;
  RowCount := SourceParameters.RowCount;
end;

procedure TdxInsertTableFormController.ApplyChanges;
begin
  SourceParameters.ColumnCount := ColumnCount;
  SourceParameters.RowCount := RowCount;
end;

end.
