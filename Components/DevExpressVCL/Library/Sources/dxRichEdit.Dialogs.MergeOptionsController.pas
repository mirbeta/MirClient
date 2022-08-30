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

unit dxRichEdit.Dialogs.MergeOptionsController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Graphics, Generics.Defaults, Generics.Collections,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.MailMerge,
  dxRichEdit.Utils.Properties;

type

  { TdxMergeOptionsFormControllerParameters }

  TdxMergeOptionsFormControllerParameters = class(TdxFormControllerParameters)
  private
    FMergeRecordsParameters: TdxMergeRecordsParameters;
  public
    constructor Create(const AControl: IdxRichEditControl; const AMergeRecordsParameters: TdxMergeRecordsParameters);
    property MergeRecordsParameters: TdxMergeRecordsParameters read FMergeRecordsParameters;
  end;

  { TdxMergeOptionsFormController }

  TdxMergeOptionsFormController = class(TdxFormController)
  private
    FControl: IdxRichEditControl;
    FMergeDestination: TdxMergeDestination;
    FMergeRecords: TdxMergeRecords;
    function GetMergeRecordsParameters: TdxMergeRecordsParameters;
  public
    constructor Create(AControllerParameters: TdxMergeOptionsFormControllerParameters);
    procedure ApplyChanges; override;

    property Control: IdxRichEditControl read FControl;
    property MergeRecordsParameters: TdxMergeRecordsParameters read GetMergeRecordsParameters;
    property MergeDestination: TdxMergeDestination read FMergeDestination write FMergeDestination;
    property MergeRecords: TdxMergeRecords read FMergeRecords write FMergeRecords;
  end;

implementation

uses
  dxRichEdit.Utils.Types;

{ TdxMergeOptionsFormControllerParameters }

constructor TdxMergeOptionsFormControllerParameters.Create(const AControl: IdxRichEditControl;
  const AMergeRecordsParameters: TdxMergeRecordsParameters);
begin
  inherited Create(AControl);
  FMergeRecordsParameters := AMergeRecordsParameters;
end;

{ TdxMergeOptionsFormController }

constructor TdxMergeOptionsFormController.Create(AControllerParameters: TdxMergeOptionsFormControllerParameters);
begin
  inherited Create;
  FControl := AControllerParameters.Control;
  FMergeDestination := AControllerParameters.MergeRecordsParameters.MergeDestination;
  FMergeRecords := AControllerParameters.MergeRecordsParameters.MergeRecords;
end;

function TdxMergeOptionsFormController.GetMergeRecordsParameters: TdxMergeRecordsParameters;
begin
  Result := TdxMergeRecordsParameters.Create(MergeRecords, MergeDestination);
end;

procedure TdxMergeOptionsFormController.ApplyChanges;
begin
end;

end.
