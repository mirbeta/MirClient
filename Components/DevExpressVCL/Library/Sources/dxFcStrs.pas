{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressFlowChart                                         }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSFLOWCHART AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE end USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxFcStrs;

interface

uses
  dxflchrt;

{$I cxVer.inc}

resourcestring
  sdxFlowChartArrowStyleNone = 'None';
  sdxFlowChartArrowStyleArrow = 'Arrow';
  sdxFlowChartArrowStyleEllipseArrow = 'Ellipse arrow';
  sdxFlowChartArrowStyleRectArrow = 'Rectangular arrow';
  sdxFlowChartArrowStyleClosedASMEarrow = 'Closed ASME arrow';
  sdxFlowChartArrowStyleFilledASMEarrow = 'Filled ASME arrow';
  sdxFlowChartArrowStyleClosedArrow = 'Closed arrow';
  sdxFlowChartArrowStyleFilledArrow = 'Filled arrow';
  sdxFlowChartArrowStyleIndentedClosedArrow = 'Indented closed arrow';
  sdxFlowChartArrowStyleIndentedFilledArrow = 'Indented filled arrow';
  sdxFlowChartArrowStyleOutdentedClosedArrow = 'Outdented closed arrow';
  sdxFlowChartArrowStyleOutdentedFilledArrow = 'Outdented filled arrow';
  sdxFlowChartArrowStyleClosedDoubleArrow = 'Closed double arrow';
  sdxFlowChartArrowStyleFilledDoubleArrow = 'Filled double arrow';
  sdxFlowChartArrowStyleDiamond = 'Diamond';
  sdxFlowChartArrowStyleFilledDiamond = 'Filled diamond';
  sdxFlowChartArrowStyleClosedDiamond = 'Closed diamond';
  sdxFlowChartArrowStyleFilledClosedDiamond = 'Filled closed diamond';
  sdxFlowChartArrowStyleDimensionLine = 'Dimension line';
  sdxFlowChartArrowStyleBackslash = 'Backslash';
  sdxFlowChartArrowStyleOpenOneDash = 'Open one dash';
  sdxFlowChartArrowStyleOpenTwoDash = 'Open two dash';
  sdxFlowChartArrowStyleOpenThreeDash = 'Open three dash';
  sdxFlowChartArrowStyleClosedOneDash = 'Closed one dash';
  sdxFlowChartArrowStyleClosedTwoDash = 'Closed two dash';
  sdxFlowChartArrowStyleClosedThreeDash = 'Closed three dash';
  sdxFlowChartArrowStyleFilledOneDash = 'Filled one dash';
  sdxFlowChartArrowStyleFilledTwoDash = 'Filled two dash';
  sdxFlowChartArrowStyleFilledThreeDash = 'Filled three dash';

  sdxFlowChartArrowSizeCustom = 'Custom...';
  sdxFlowChartArrowSizeSmall = 'Small';
  sdxFlowChartArrowSizeMedium = 'Medium';
  sdxFlowChartArrowSizeLarge = 'Large';
  sdxFlowChartArrowSizeExtraLarge = 'ExtraLarge';
  sdxFlowChartArrowSizeHuge = 'Huge';

  sdxFlowChartConnectionEditorArrowColor = 'Arrow Color';
  sdxFlowChartConnectionEditorArrowSize = 'Arrow Size';
  sdxFlowChartConnectionEditorArrowStyle = 'Arrow Style';
  sdxFlowChartConnectionEditorCaption = 'Edit Connection';
  sdxFlowChartConnectionEditorColor = 'Color';
  sdxFlowChartConnectionEditorDestination = 'Destination';
  sdxFlowChartConnectionEditorLinkedPoint = 'Linked Point';
  sdxFlowChartConnectionEditorSource = 'Source';
  sdxFlowChartConnectionEditorText = 'Text';
  sdxFlowChartConnectionEditorTextFontHint = 'Text Font';

  sdxFlowChartDialogButtonOk = '&Ok';
  sdxFlowChartDialogButtonCancel = '&Cancel';

  sdxFlowChartBorderStyleAdjust = 'Adjust';
  sdxFlowChartBorderStyleBottom = 'Bottom';
  sdxFlowChartBorderStyleDiagonal = 'Diagonal';
  sdxFlowChartBorderStyleFlat = 'Flat';
  sdxFlowChartBorderStyleLeft = 'Left';
  sdxFlowChartBorderStyleMiddle = 'Middle';
  sdxFlowChartBorderStyleMono = 'Mono';
  sdxFlowChartBorderStyleRight = 'Right';
  sdxFlowChartBorderStyleSoft = 'Soft';
  sdxFlowChartBorderStyleTop = 'Top';

  sdxFlowChartEdgeStyleRaisedIn = 'Raised inner edge';
  sdxFlowChartEdgeStyleRaisedOut = 'Raised outer edge';
  sdxFlowChartEdgeStyleSunkenIn = 'Sunken inner edge';
  sdxFlowChartEdgeStyleSunkenOut = 'Sunken outer edge';

  sdxFlowChartEditorChildItem = 'Child Item of the %s';
  sdxFlowChartEditorConnection = 'Connect';
  sdxFlowChartEditorConnectionArrowDestinationHint = 'Destination Arrow';
  sdxFlowChartEditorConnectionArrowDestinationSizeHint = 'Destination Arrow Size';
  sdxFlowChartEditorConnectionArrowSourceHint = 'Source Arrow';
  sdxFlowChartEditorConnectionArrowSourceSizeHint = 'Source Arrow Size';
  sdxFlowChartEditorConnectionLinkedPointDestinationHint = 'Linked point of Destination object';
  sdxFlowChartEditorConnectionLinkedPointSourceHint = 'Linked point of Source object';
  sdxFlowChartEditorConnectionStyleHint = 'Line Style';
  sdxFlowChartEditorConnectionTextFontHint = 'Text Font';
  sdxFlowChartEditorCreate = 'Create';
  sdxFlowChartEditorCreateConnectionHint = 'Connection';
  sdxFlowChartEditorCreateObjectHint = 'Object';
  sdxFlowChartEditorEdit = '&Edit';
  sdxFlowChartEditorEditBringToFront = 'Bring To &Front';
  sdxFlowChartEditorEditClearSelection = 'Cl&ear Selection';
  sdxFlowChartEditorEditCopy = '&Copy';
  sdxFlowChartEditorEditCut = 'Cu&t';
  sdxFlowChartEditorEditDelete = '&Delete';
  sdxFlowChartEditorEditPaste = '&Paste';
  sdxFlowChartEditorEditSelectAll = 'Se&lect All';
  sdxFlowChartEditorEditSendToBack = 'Send To &Back';
  sdxFlowChartEditorEditUndo = '&Undo';
  sdxFlowChartEditorFile = '&File';
  sdxFlowChartEditorFileOpen = '&Open';
  sdxFlowChartEditorFileSave = 'Save &As...';
  sdxFlowChartEditorFitHint = 'Fit';
  sdxFlowChartEditorHelp = '&Help';
  sdxFlowChartEditorHelpContents = '&Contents';
  sdxFlowChartEditorMainItemOfUnion = 'Main Item of the Union %d';
  sdxFlowChartEditorObject = 'Object';
  sdxFlowChartEditorObjectImagePositionHint = 'Image Position';
  sdxFlowChartEditorObjectLineWidthHint = 'Line Width';
  sdxFlowChartEditorObjectShapeStyleHint = 'Shape Style';
  sdxFlowChartEditorObjectTextFontHint = 'Text Font';
  sdxFlowChartEditorObjectTextPositionHint = 'Text Position';
  sdxFlowChartEditorOptions = '&Options';
  sdxFlowChartEditorOptionsDynamicMoving = 'Dynamic &Moving';
  sdxFlowChartEditorOptionsDynamicSizing = 'Dynamic &Sizing';
  sdxFlowChartEditorPixels = '%d px.';
  sdxFlowChartEditorPoint = '%d Point';
  sdxFlowChartEditorProperties = '&Properties';
  sdxFlowChartEditorUnions = '&Unions';
  sdxFlowChartEditorUnionsAdd = 'Add To Union';
  sdxFlowChartEditorUnionsClear = 'Clear Union';
  sdxFlowChartEditorUnionsClearAll = 'Clear All Unions';
  sdxFlowChartEditorUnionsNew = 'New Union';
  sdxFlowChartEditorUnionsRemove = 'Remove From Union';
  sdxFlowChartEditorView = '&View';
  sdxFlowChartEditorViewActualSize = '&Actual Size';
  sdxFlowChartEditorViewAntialiasing = '&Antialiasing';
  sdxFlowChartEditorViewFit = '&Fit';
  sdxFlowChartEditorViewZoomIn = 'Zoom &In';
  sdxFlowChartEditorViewZoomOut = 'Zoom &Out';
  sdxFlowChartEditorZoomHint = 'Zoom';

  sdxFlowChartObjectEditorBackgroundColor = 'Background Color';
  sdxFlowChartObjectEditorBorderStyle = 'Border Style';
  sdxFlowChartObjectEditorCaption = 'Edit Object';
  sdxFlowChartObjectEditorEdgeStyle = 'Edge Style';
  sdxFlowChartObjectEditorFrameTab = 'Frame';
  sdxFlowChartObjectEditorGeneralTab = 'General';
  sdxFlowChartObjectEditorHeight = 'Height';
  sdxFlowChartObjectEditorImageClear = 'Clear Image';
  sdxFlowChartObjectEditorImageLayout = 'Image Layout';
  sdxFlowChartObjectEditorImageTab = 'Image';
  sdxFlowChartObjectEditorLineWidth = 'Line Width';
  sdxFlowChartObjectEditorShapeColor = 'Shape Color';
  sdxFlowChartObjectEditorShapeType = 'Shape Type';
  sdxFlowChartObjectEditorText = 'Text';
  sdxFlowChartObjectEditorTextLayout = 'Text Layout';
  sdxFlowChartObjectEditorTransparent = 'Transparent';
  sdxFlowChartObjectEditorWidth = 'Width';

  sdxFlowChartLayoutTopLeft = 'Top-Left';
  sdxFlowChartLayoutTop = 'Top';
  sdxFlowChartLayoutTopRight = 'Top-Right';
  sdxFlowChartLayoutLeft = 'Left';
  sdxFlowChartLayoutCenter = 'Center';
  sdxFlowChartLayoutRight = 'Right';
  sdxFlowChartLayoutBottomLeft = 'Bottom-Left';
  sdxFlowChartLayoutBottom = 'Bottom';
  sdxFlowChartLayoutBottomRight = 'Bottom-Right';

  sdxFlowChartShapeTypeNone = 'None';
  sdxFlowChartShapeTypeRect = 'Rectangle';
  sdxFlowChartShapeTypeEllipse = 'Ellipse';
  sdxFlowChartShapeTypeRoundRect = 'Round Rect';
  sdxFlowChartShapeTypeDiamond = 'Diamond';
  sdxFlowChartShapeTypeNorthTriangle = 'North Triangle';
  sdxFlowChartShapeTypeSouthTriangle = 'South Triangle';
  sdxFlowChartShapeTypeEastTriangle = 'East Triangle';
  sdxFlowChartShapeTypeWestTriangle = 'West Triangle';
  sdxFlowChartShapeTypeHexagon = 'Hexagon';

  sdxFlowChartUnion = 'Union';
  sdxFlowChartUnions = 'Unions';
  sdxFlowChartUnionEditorCaption = 'Select Union';

  sdxFlowChartConnectionStyleStraight = 'Straight';
  sdxFlowChartConnectionStyleCurved = 'Curved';
  sdxFlowChartConnectionStyleRectHorizontal = 'Right Angle Horizontal';
  sdxFlowChartConnectionStyleRectVertical = 'Right Angle Vertical';

  sdxFlowChart_Search_Shapes_Null_Text = 'Search shapes...';
  sdxFlowChart_QuickShapesCaption = 'Quick Shapes';
  sdxFlowChart_No_Stencils_Open = 'There are no stencils open.';
  sdxFlowChart_No_Shapes_Found = 'No matches';
  sdxFlowChart_More_Shapes = 'More Shapes';

  sdxFlowChart_BasicShapesCaption = 'Basic Shapes';
  sdxFlowChart_BasicShapes_Rectangle = 'Rectangle';
  sdxFlowChart_BasicShapes_Ellipse = 'Ellipse';
  sdxFlowChart_BasicShapes_Triangle = 'Triangle';
  sdxFlowChart_BasicShapes_RightTriangle = 'Right Triangle';

  sdxFlowChart_BasicShapes_Pentagon = 'Pentagon';
  sdxFlowChart_BasicShapes_Hexagon = 'Hexagon';
  sdxFlowChart_BasicShapes_Heptagon = 'Heptagon';
  sdxFlowChart_BasicShapes_Octagon = 'Octagon';
  sdxFlowChart_BasicShapes_Decagon = 'Decagon';

  sdxFlowChart_BasicShapes_Can = 'Can';
  sdxFlowChart_BasicShapes_Parallelogram = 'Parallelogram';
  sdxFlowChart_BasicShapes_Trapezoid = 'Trapezoid';
  sdxFlowChart_BasicShapes_Diamond = 'Diamond';
  sdxFlowChart_BasicShapes_Cross = 'Cross';
  sdxFlowChart_BasicShapes_Chevron = 'Chevron';
  sdxFlowChart_BasicShapes_Cube = 'Cube';

  sdxFlowChart_BasicShapes_Star4 = '4-Point Star';
  sdxFlowChart_BasicShapes_Star5 = '5-Point Star';
  sdxFlowChart_BasicShapes_Star6 = '6-Point Star';
  sdxFlowChart_BasicShapes_Star7 = '7-Point Star';
  sdxFlowChart_BasicShapes_Star16 = '16-Point Star';
  sdxFlowChart_BasicShapes_Star24 = '24-Point Star';
  sdxFlowChart_BasicShapes_Star32 = '32-Point Star';

  sdxFlowChart_BasicShapes_RoundedRectangle = 'Rounded Rectangle';
  sdxFlowChart_BasicShapes_SingleSnipCornerRectangle = 'Single Snip Corner Rectangle';
  sdxFlowChart_BasicShapes_SnipSameSideCornerRectangle = 'Snip Same Side Corner Rectangle';
  sdxFlowChart_BasicShapes_SnipDiagonalCornerRectangle = 'Snip Diagonal Corner Rectangle';

  sdxFlowChart_BasicShapes_SingleRoundCornerRectangle = 'Single Round Corner Rectangle';
  sdxFlowChart_BasicShapes_RoundSameSideCornerRectangle = 'Round Same Side Corner Rectangle';
  sdxFlowChart_BasicShapes_RoundDiagonalCornerRectangle = 'Round Diagonal Corner Rectangle';

  sdxFlowChart_BasicShapes_SnipAndRoundSingleCornerRectangle = 'Snip And Round Single Corner Rectangle';
  sdxFlowChart_BasicShapes_SnipCornerRectangle = 'Snip Corner Rectangle';
  sdxFlowChart_BasicShapes_RoundCornerRectangle = 'Round Corner Rectangle';
  sdxFlowChart_BasicShapes_SnipAndRoundCornerRectangle = 'Snip And Round Corner Rectangle';
  sdxFlowChart_BasicShapes_Plaque = 'Plaque';

  sdxFlowChart_BasicShapes_Frame = 'Frame';
  sdxFlowChart_BasicShapes_FrameCorner = 'Frame Corner';

  sdxFlowChart_BasicShapes_LShape = 'L Shape';
  sdxFlowChart_BasicShapes_DiagonalStripe = 'Diagonal Stripe';

  sdxFlowChart_BasicShapes_Donut = 'Donut';
  sdxFlowChart_BasicShapes_NoSymbol = 'NoSymbol';

  sdxFlowChart_BasicShapes_LeftParenthesis = 'Left Parenthesis';
  sdxFlowChart_BasicShapes_RightParenthesis = 'Right Parenthesis';
  sdxFlowChart_BasicShapes_LeftBrace = 'Left Brace';
  sdxFlowChart_BasicShapes_RightBrace = 'Right Brace';

  sdxFlowChart_BasicFlowchartShapesCaption = 'Basic Flowchart Shapes';

  sdxFlowChart_BasicFlowchartShapes_Process = 'Process';
  sdxFlowChart_BasicFlowchartShapes_Decision = 'Decision';
  sdxFlowChart_BasicFlowchartShapes_Subprocess = 'Subprocess';
  sdxFlowChart_BasicFlowchartShapes_StartEnd = 'Start/End';
  sdxFlowChart_BasicFlowchartShapes_Document = 'Document';
  sdxFlowChart_BasicFlowchartShapes_Data = 'Data';
  sdxFlowChart_BasicFlowchartShapes_Database = 'Database';
  sdxFlowChart_BasicFlowchartShapes_ExternalData = 'External Data';
  sdxFlowChart_BasicFlowchartShapes_Custom1 = 'Custom1';
  sdxFlowChart_BasicFlowchartShapes_Custom2 = 'Custom2';
  sdxFlowChart_BasicFlowchartShapes_Custom3 = 'Custom3';
  sdxFlowChart_BasicFlowchartShapes_Custom4 = 'Custom4';
  sdxFlowChart_BasicFlowchartShapes_OnPageReference = 'On-page reference';
  sdxFlowChart_BasicFlowchartShapes_OffPageReference = 'Off-page reference';

  sdxFlowChart_SDLDiagramShapesCaption = 'SDL Diagram Shapes';
  sdxFlowChart_SDLDiagramShapes_Start = 'Start';
  sdxFlowChart_SDLDiagramShapes_VariableStart = 'Variable Start';
  sdxFlowChart_SDLDiagramShapes_Procedure = 'Procedure';
  sdxFlowChart_SDLDiagramShapes_VariableProcedure = 'Variable Procedure';
  sdxFlowChart_SDLDiagramShapes_CreateRequest = 'CreateRequest';
  sdxFlowChart_SDLDiagramShapes_Alternative = 'Alternative';
  sdxFlowChart_SDLDiagramShapes_Return = 'Return';
  sdxFlowChart_SDLDiagramShapes_Decision1 = 'Decision1';
  sdxFlowChart_SDLDiagramShapes_MessageFromUser = 'Message from user';
  sdxFlowChart_SDLDiagramShapes_PrimitiveFromCallControl = 'Primitive from call control';
  sdxFlowChart_SDLDiagramShapes_Decision2 = 'Decision2';
  sdxFlowChart_SDLDiagramShapes_MessageToUser = 'Message to user';
  sdxFlowChart_SDLDiagramShapes_PrimitiveToCallControl = 'Primitive to call control';
  sdxFlowChart_SDLDiagramShapes_Save = 'Save';
  sdxFlowChart_SDLDiagramShapes_OnPageReference = 'On page reference';
  sdxFlowChart_SDLDiagramShapes_OffPageReference = 'Off page reference';
  sdxFlowChart_SDLDiagramShapes_Document = 'Document';
  sdxFlowChart_SDLDiagramShapes_DiskStorage = 'Disk storage';
  sdxFlowChart_SDLDiagramShapes_DividedProcess = 'Divided process';
  sdxFlowChart_SDLDiagramShapes_DividedEvent = 'Divided event';
  sdxFlowChart_SDLDiagramShapes_Terminator = 'Terminator';

  sdxFlowChart_SoftwareIconsCaption = 'Software Icons';
  sdxFlowChart_SoftwareIcons_Back = 'Back';
  sdxFlowChart_SoftwareIcons_Forward = 'Forward';
  sdxFlowChart_SoftwareIcons_Expand = 'Expand';
  sdxFlowChart_SoftwareIcons_Collapse = 'Collapse';
  sdxFlowChart_SoftwareIcons_Add = 'Add';
  sdxFlowChart_SoftwareIcons_Remove = 'Remove';
  sdxFlowChart_SoftwareIcons_ZoomIn = 'ZoomIn';
  sdxFlowChart_SoftwareIcons_ZoomOut = 'ZoomOut';
  sdxFlowChart_SoftwareIcons_Lock = 'Lock';
  sdxFlowChart_SoftwareIcons_Permission = 'Permission';
  sdxFlowChart_SoftwareIcons_Sort = 'Sort';
  sdxFlowChart_SoftwareIcons_Filter = 'Filter';
  sdxFlowChart_SoftwareIcons_Tools = 'Tools';
  sdxFlowChart_SoftwareIcons_Properties = 'Properties';
  sdxFlowChart_SoftwareIcons_Calendar = 'Calendar';
  sdxFlowChart_SoftwareIcons_Document = 'Document';
  sdxFlowChart_SoftwareIcons_Database = 'Database';
  sdxFlowChart_SoftwareIcons_HardDrive = 'HardDrive';
  sdxFlowChart_SoftwareIcons_Network = 'Network';

  sdxFlowChart_DecorativeShapesCaption = 'Decorative Shapes';
  sdxFlowChart_DecorativeShapes_LightningBolt = 'Lightning Bolt';
  sdxFlowChart_DecorativeShapes_Moon = 'Moon';
  sdxFlowChart_DecorativeShapes_Wave = 'Wave';
  sdxFlowChart_DecorativeShapes_DoubleWave = 'Double Wave';
  sdxFlowChart_DecorativeShapes_VerticalScroll = 'Vertical Scroll';
  sdxFlowChart_DecorativeShapes_HorizontalScroll = 'Horizontal Scroll';
  sdxFlowChart_DecorativeShapes_Heart = 'Heart';
  sdxFlowChart_DecorativeShapes_DownRibbon = 'Down Ribbon';
  sdxFlowChart_DecorativeShapes_UpRibbon = 'Up Ribbon';
  sdxFlowChart_DecorativeShapes_Cloud = 'Cloud';

  sdxFlowChart_ArrowShapesCaption = 'Arrow Shapes';
  sdxFlowChart_ArrowShapes_SimpleArrow = 'Simple Arrow';
  sdxFlowChart_ArrowShapes_SimpleDoubleArrow = 'Simple Double Arrow';
  sdxFlowChart_ArrowShapes_ModernArrow = 'Modern Arrow';
  sdxFlowChart_ArrowShapes_FlexibleArrow = 'Flexible Arrow';
  sdxFlowChart_ArrowShapes_BentArrow = 'Bent Arrow';
  sdxFlowChart_ArrowShapes_UTurnArrow = 'U Turn Arrow';
  sdxFlowChart_ArrowShapes_SharpBentArrow = 'Sharp Bent Arrow';
  sdxFlowChart_ArrowShapes_CurvedRightArrow = 'Curved Right Arrow';
  sdxFlowChart_ArrowShapes_CurvedLeftArrow = 'Curved Left Arrow';
  sdxFlowChart_ArrowShapes_NotchedArrow = 'Notched Arrow';
  sdxFlowChart_ArrowShapes_StripedArrow = 'Striped Arrow';
  sdxFlowChart_ArrowShapes_BlockArrow = 'Block Arrow';
  sdxFlowChart_ArrowShapes_CircularArrow = 'Circular Arrow';
  sdxFlowChart_ArrowShapes_QuadArrow = 'Quad Arrow';
  sdxFlowChart_ArrowShapes_LeftRightUpArrow = 'Left Right Up Arrow';
  sdxFlowChart_ArrowShapes_LeftRightArrowBlock = 'Left Right Arrow Block';
  sdxFlowChart_ArrowShapes_QuadArrowBlock = 'Quad Arrow Block';

  sdxFlowChart_Arrow_Open90 = 'Open 90 arrow';
  sdxFlowChart_Arrow_Filled90 = 'Filled 90 arrow';
  sdxFlowChart_Arrow_ClosedDot = 'Closed dot';
  sdxFlowChart_Arrow_FilledDot = 'Filled dot';
  sdxFlowChart_Arrow_OpenFletch = 'Open fletch';
  sdxFlowChart_Arrow_FilledFletch = 'Filled fletch';
  sdxFlowChart_Arrow_Diamond = 'Diamond';
  sdxFlowChart_Arrow_FilledDiamond = 'Filled diamond';
  sdxFlowChart_Arrow_ClosedDiamond = 'Closed diamond';
  sdxFlowChart_Arrow_IndentedFilledArrow = 'Indented filled arrow';
  sdxFlowChart_Arrow_OutdentedFilledArrow = 'Outdented filled arrow';
  sdxFlowChart_Arrow_FilledSquare = 'Filled square';
  sdxFlowChart_Arrow_ClosedASMEArrow = 'Closed ASME arrow';
  sdxFlowChart_Arrow_FilledDoubleArrow = 'Filled double arrow';
  sdxFlowChart_Arrow_ClosedDoubleArrow = 'Closed double arrow';

  sdxFlowChartEditorLineSolid = 'Solid';
  sdxFlowChartEditorLineDashed = 'Dashed';
  sdxFlowChartEditorLineDotted = 'Dotted';
  sdxFlowChartEditorLineDashDotted = 'Dash-Dotted';
  sdxFlowChartEditorLineDashDoubleDotted = 'Dash-Double-Dotted';

const
  dxFlowChartArrowStyleNamesMap: array [TdxFcaType] of Pointer = (
    @sdxFlowChartArrowStyleNone,
    @sdxFlowChartArrowStyleArrow,
    @sdxFlowChartArrowStyleEllipseArrow,
    @sdxFlowChartArrowStyleRectArrow,
    @sdxFlowChartArrowStyleClosedASMEarrow,
    @sdxFlowChartArrowStyleFilledASMEarrow,
    @sdxFlowChartArrowStyleClosedArrow,
    @sdxFlowChartArrowStyleFilledArrow,
    @sdxFlowChartArrowStyleIndentedClosedArrow,
    @sdxFlowChartArrowStyleIndentedFilledArrow,
    @sdxFlowChartArrowStyleOutdentedClosedArrow,
    @sdxFlowChartArrowStyleOutdentedFilledArrow,
    @sdxFlowChartArrowStyleClosedDoubleArrow,
    @sdxFlowChartArrowStyleFilledDoubleArrow,
    @sdxFlowChartArrowStyleDiamond,
    @sdxFlowChartArrowStyleFilledDiamond,
    @sdxFlowChartArrowStyleClosedDiamond,
    @sdxFlowChartArrowStyleFilledClosedDiamond,
    @sdxFlowChartArrowStyleDimensionLine,
    @sdxFlowChartArrowStyleBackslash,
    @sdxFlowChartArrowStyleOpenOneDash,
    @sdxFlowChartArrowStyleOpenTwoDash,
    @sdxFlowChartArrowStyleOpenThreeDash,
    @sdxFlowChartArrowStyleClosedOneDash,
    @sdxFlowChartArrowStyleClosedTwoDash,
    @sdxFlowChartArrowStyleClosedThreeDash,
    @sdxFlowChartArrowStyleFilledOneDash,
    @sdxFlowChartArrowStyleFilledTwoDash,
    @sdxFlowChartArrowStyleFilledThreeDash
   );

  dxFlowChartConnectionStyleNamesMap: array[TdxFclStyle] of Pointer = (
    @sdxFlowChartConnectionStyleStraight, @sdxFlowChartConnectionStyleCurved,
    @sdxFlowChartConnectionStyleRectHorizontal, @sdxFlowChartConnectionStyleRectVertical);

  dxFlowChartShapeNamesMap: array[TdxFcShapeType] of Pointer = (
    @sdxFlowChartShapeTypeNone, @sdxFlowChartShapeTypeRect,
    @sdxFlowChartShapeTypeEllipse, @sdxFlowChartShapeTypeRoundRect,
    @sdxFlowChartShapeTypeDiamond, @sdxFlowChartShapeTypeNorthTriangle,
    @sdxFlowChartShapeTypeSouthTriangle, @sdxFlowChartShapeTypeEastTriangle,
    @sdxFlowChartShapeTypeWestTriangle, @sdxFlowChartShapeTypeHexagon, nil, nil);

  dxFlowChartLayoutNamesMap: array[0..8] of Pointer = (
    @sdxFlowChartLayoutTopLeft, @sdxFlowChartLayoutTop, @sdxFlowChartLayoutTopRight,
    @sdxFlowChartLayoutLeft, @sdxFlowChartLayoutCenter, @sdxFlowChartLayoutRight,
    @sdxFlowChartLayoutBottomLeft, @sdxFlowChartLayoutBottom, @sdxFlowChartLayoutBottomRight);

  dxFlowChartConnectionArrowSizeNamesMap: array[TdxFcaSize] of Pointer = (
    @sdxFlowChartArrowSizeCustom,
    @sdxFlowChartArrowSizeSmall,
    @sdxFlowChartArrowSizeMedium,
    @sdxFlowChartArrowSizeLarge,
    @sdxFlowChartArrowSizeExtraLarge,
    @sdxFlowChartArrowSizeHuge);

implementation

uses
  dxCore;

procedure AddFlowChartVisioResourceStringNames(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxFlowChart_Search_Shapes_Null_Text', @sdxFlowChart_Search_Shapes_Null_Text);
  AProduct.Add('sdxFlowChart_QuickShapesCaption', @sdxFlowChart_QuickShapesCaption);
  AProduct.Add('sdxFlowChart_No_Stencils_Open', @sdxFlowChart_No_Stencils_Open);
  AProduct.Add('sdxFlowChart_No_Shapes_Found', @sdxFlowChart_No_Shapes_Found);
  AProduct.Add('sdxFlowChart_More_Shapes', @sdxFlowChart_More_Shapes);

  AProduct.Add('sdxFlowChart_BasicShapesCaption', @sdxFlowChart_BasicShapesCaption);
  AProduct.Add('sdxFlowChart_BasicShapes_Rectangle', @sdxFlowChart_BasicShapes_Rectangle);
  AProduct.Add('sdxFlowChart_BasicShapes_Ellipse', @sdxFlowChart_BasicShapes_Ellipse);
  AProduct.Add('sdxFlowChart_BasicShapes_Triangle', @sdxFlowChart_BasicShapes_Triangle);
  AProduct.Add('sdxFlowChart_BasicShapes_RightTriangle', @sdxFlowChart_BasicShapes_RightTriangle);

  AProduct.Add('sdxFlowChart_BasicShapes_Pentagon', @sdxFlowChart_BasicShapes_Pentagon);
  AProduct.Add('sdxFlowChart_BasicShapes_Hexagon', @sdxFlowChart_BasicShapes_Hexagon);
  AProduct.Add('sdxFlowChart_BasicShapes_Heptagon', @sdxFlowChart_BasicShapes_Heptagon);
  AProduct.Add('sdxFlowChart_BasicShapes_Octagon', @sdxFlowChart_BasicShapes_Octagon);
  AProduct.Add('sdxFlowChart_BasicShapes_Decagon', @sdxFlowChart_BasicShapes_Decagon);

  AProduct.Add('sdxFlowChart_BasicShapes_Can', @sdxFlowChart_BasicShapes_Can);
  AProduct.Add('sdxFlowChart_BasicShapes_Parallelogram', @sdxFlowChart_BasicShapes_Parallelogram);
  AProduct.Add('sdxFlowChart_BasicShapes_Trapezoid', @sdxFlowChart_BasicShapes_Trapezoid);
  AProduct.Add('sdxFlowChart_BasicShapes_Diamond', @sdxFlowChart_BasicShapes_Diamond);
  AProduct.Add('sdxFlowChart_BasicShapes_Cross', @sdxFlowChart_BasicShapes_Cross);
  AProduct.Add('sdxFlowChart_BasicShapes_Chevron', @sdxFlowChart_BasicShapes_Chevron);
  AProduct.Add('sdxFlowChart_BasicShapes_Cube', @sdxFlowChart_BasicShapes_Cube);

  AProduct.Add('sdxFlowChart_BasicShapes_Star4', @sdxFlowChart_BasicShapes_Star4);
  AProduct.Add('sdxFlowChart_BasicShapes_Star5', @sdxFlowChart_BasicShapes_Star5);
  AProduct.Add('sdxFlowChart_BasicShapes_Star6', @sdxFlowChart_BasicShapes_Star6);
  AProduct.Add('sdxFlowChart_BasicShapes_Star7', @sdxFlowChart_BasicShapes_Star7);
  AProduct.Add('sdxFlowChart_BasicShapes_Star16', @sdxFlowChart_BasicShapes_Star16);
  AProduct.Add('sdxFlowChart_BasicShapes_Star24', @sdxFlowChart_BasicShapes_Star24);
  AProduct.Add('sdxFlowChart_BasicShapes_Star32', @sdxFlowChart_BasicShapes_Star32);

  AProduct.Add('sdxFlowChart_BasicShapes_RoundedRectangle', @sdxFlowChart_BasicShapes_RoundedRectangle);
  AProduct.Add('sdxFlowChart_BasicShapes_SingleSnipCornerRectangle', @sdxFlowChart_BasicShapes_SingleSnipCornerRectangle);
  AProduct.Add('sdxFlowChart_BasicShapes_SnipSameSideCornerRectangle', @sdxFlowChart_BasicShapes_SnipSameSideCornerRectangle);
  AProduct.Add('sdxFlowChart_BasicShapes_SnipDiagonalCornerRectangle', @sdxFlowChart_BasicShapes_SnipDiagonalCornerRectangle);

  AProduct.Add('sdxFlowChart_BasicShapes_SingleRoundCornerRectangle', @sdxFlowChart_BasicShapes_SingleRoundCornerRectangle);
  AProduct.Add('sdxFlowChart_BasicShapes_RoundSameSideCornerRectangle', @sdxFlowChart_BasicShapes_RoundSameSideCornerRectangle);
  AProduct.Add('sdxFlowChart_BasicShapes_RoundDiagonalCornerRectangle', @sdxFlowChart_BasicShapes_RoundDiagonalCornerRectangle);

  AProduct.Add('sdxFlowChart_BasicShapes_SnipAndRoundSingleCornerRectangle', @sdxFlowChart_BasicShapes_SnipAndRoundSingleCornerRectangle);
  AProduct.Add('sdxFlowChart_BasicShapes_SnipCornerRectangle', @sdxFlowChart_BasicShapes_SnipCornerRectangle);
  AProduct.Add('sdxFlowChart_BasicShapes_RoundCornerRectangle', @sdxFlowChart_BasicShapes_RoundCornerRectangle);
  AProduct.Add('sdxFlowChart_BasicShapes_SnipAndRoundCornerRectangle', @sdxFlowChart_BasicShapes_SnipAndRoundCornerRectangle);
  AProduct.Add('sdxFlowChart_BasicShapes_Plaque', @sdxFlowChart_BasicShapes_Plaque);

  AProduct.Add('sdxFlowChart_BasicShapes_Frame', @sdxFlowChart_BasicShapes_Frame);
  AProduct.Add('sdxFlowChart_BasicShapes_FrameCorner', @sdxFlowChart_BasicShapes_FrameCorner);

  AProduct.Add('sdxFlowChart_BasicShapes_LShape', @sdxFlowChart_BasicShapes_LShape);
  AProduct.Add('sdxFlowChart_BasicShapes_DiagonalStripe', @sdxFlowChart_BasicShapes_DiagonalStripe);

  AProduct.Add('sdxFlowChart_BasicShapes_Donut', @sdxFlowChart_BasicShapes_Donut);
  AProduct.Add('sdxFlowChart_BasicShapes_NoSymbol', @sdxFlowChart_BasicShapes_NoSymbol);

  AProduct.Add('sdxFlowChart_BasicShapes_LeftParenthesis', @sdxFlowChart_BasicShapes_LeftParenthesis);
  AProduct.Add('sdxFlowChart_BasicShapes_RightParenthesis', @sdxFlowChart_BasicShapes_RightParenthesis);
  AProduct.Add('sdxFlowChart_BasicShapes_LeftBrace', @sdxFlowChart_BasicShapes_LeftBrace);
  AProduct.Add('sdxFlowChart_BasicShapes_RightBrace', @sdxFlowChart_BasicShapes_RightBrace);

  AProduct.Add('sdxFlowChart_BasicFlowchartShapesCaption', @sdxFlowChart_BasicFlowchartShapesCaption);

  AProduct.Add('sdxFlowChart_BasicFlowchartShapes_Process', @sdxFlowChart_BasicFlowchartShapes_Process);
  AProduct.Add('sdxFlowChart_BasicFlowchartShapes_Decision', @sdxFlowChart_BasicFlowchartShapes_Decision);
  AProduct.Add('sdxFlowChart_BasicFlowchartShapes_Subprocess', @sdxFlowChart_BasicFlowchartShapes_Subprocess);
  AProduct.Add('sdxFlowChart_BasicFlowchartShapes_StartEnd', @sdxFlowChart_BasicFlowchartShapes_StartEnd);
  AProduct.Add('sdxFlowChart_BasicFlowchartShapes_Document', @sdxFlowChart_BasicFlowchartShapes_Document);
  AProduct.Add('sdxFlowChart_BasicFlowchartShapes_Data', @sdxFlowChart_BasicFlowchartShapes_Data);
  AProduct.Add('sdxFlowChart_BasicFlowchartShapes_Database', @sdxFlowChart_BasicFlowchartShapes_Database);
  AProduct.Add('sdxFlowChart_BasicFlowchartShapes_ExternalData', @sdxFlowChart_BasicFlowchartShapes_ExternalData);
  AProduct.Add('sdxFlowChart_BasicFlowchartShapes_Custom1', @sdxFlowChart_BasicFlowchartShapes_Custom1);
  AProduct.Add('sdxFlowChart_BasicFlowchartShapes_Custom2', @sdxFlowChart_BasicFlowchartShapes_Custom2);
  AProduct.Add('sdxFlowChart_BasicFlowchartShapes_Custom3', @sdxFlowChart_BasicFlowchartShapes_Custom3);
  AProduct.Add('sdxFlowChart_BasicFlowchartShapes_Custom4', @sdxFlowChart_BasicFlowchartShapes_Custom4);
  AProduct.Add('sdxFlowChart_BasicFlowchartShapes_OnPageReference', @sdxFlowChart_BasicFlowchartShapes_OnPageReference);
  AProduct.Add('sdxFlowChart_BasicFlowchartShapes_OffPageReference', @sdxFlowChart_BasicFlowchartShapes_OffPageReference);

  AProduct.Add('sdxFlowChart_SDLDiagramShapesCaption', @sdxFlowChart_SDLDiagramShapesCaption);
  AProduct.Add('sdxFlowChart_SDLDiagramShapes_Start', @sdxFlowChart_SDLDiagramShapes_Start);
  AProduct.Add('sdxFlowChart_SDLDiagramShapes_VariableStart', @sdxFlowChart_SDLDiagramShapes_VariableStart);
  AProduct.Add('sdxFlowChart_SDLDiagramShapes_Procedure', @sdxFlowChart_SDLDiagramShapes_Procedure);
  AProduct.Add('sdxFlowChart_SDLDiagramShapes_VariableProcedure', @sdxFlowChart_SDLDiagramShapes_VariableProcedure);
  AProduct.Add('sdxFlowChart_SDLDiagramShapes_CreateRequest', @sdxFlowChart_SDLDiagramShapes_CreateRequest);
  AProduct.Add('sdxFlowChart_SDLDiagramShapes_Alternative', @sdxFlowChart_SDLDiagramShapes_Alternative);
  AProduct.Add('sdxFlowChart_SDLDiagramShapes_Return', @sdxFlowChart_SDLDiagramShapes_Return);
  AProduct.Add('sdxFlowChart_SDLDiagramShapes_Decision1', @sdxFlowChart_SDLDiagramShapes_Decision1);
  AProduct.Add('sdxFlowChart_SDLDiagramShapes_MessageFromUser', @sdxFlowChart_SDLDiagramShapes_MessageFromUser);
  AProduct.Add('sdxFlowChart_SDLDiagramShapes_PrimitiveFromCallControl', @sdxFlowChart_SDLDiagramShapes_PrimitiveFromCallControl);
  AProduct.Add('sdxFlowChart_SDLDiagramShapes_Decision2', @sdxFlowChart_SDLDiagramShapes_Decision2);
  AProduct.Add('sdxFlowChart_SDLDiagramShapes_MessageToUser', @sdxFlowChart_SDLDiagramShapes_MessageToUser);
  AProduct.Add('sdxFlowChart_SDLDiagramShapes_PrimitiveToCallControl', @sdxFlowChart_SDLDiagramShapes_PrimitiveToCallControl);
  AProduct.Add('sdxFlowChart_SDLDiagramShapes_Save', @sdxFlowChart_SDLDiagramShapes_Save);
  AProduct.Add('sdxFlowChart_SDLDiagramShapes_OnPageReference', @sdxFlowChart_SDLDiagramShapes_OnPageReference);
  AProduct.Add('sdxFlowChart_SDLDiagramShapes_OffPageReference', @sdxFlowChart_SDLDiagramShapes_OffPageReference);
  AProduct.Add('sdxFlowChart_SDLDiagramShapes_Document', @sdxFlowChart_SDLDiagramShapes_Document);
  AProduct.Add('sdxFlowChart_SDLDiagramShapes_DiskStorage', @sdxFlowChart_SDLDiagramShapes_DiskStorage);
  AProduct.Add('sdxFlowChart_SDLDiagramShapes_DividedProcess', @sdxFlowChart_SDLDiagramShapes_DividedProcess);
  AProduct.Add('sdxFlowChart_SDLDiagramShapes_DividedEvent', @sdxFlowChart_SDLDiagramShapes_DividedEvent);
  AProduct.Add('sdxFlowChart_SDLDiagramShapes_Terminator', @sdxFlowChart_SDLDiagramShapes_Terminator);

  AProduct.Add('sdxFlowChart_SoftwareIconsCaption', @sdxFlowChart_SoftwareIconsCaption);
  AProduct.Add('sdxFlowChart_SoftwareIcons_Back', @sdxFlowChart_SoftwareIcons_Back);
  AProduct.Add('sdxFlowChart_SoftwareIcons_Forward', @sdxFlowChart_SoftwareIcons_Forward);
  AProduct.Add('sdxFlowChart_SoftwareIcons_Expand', @sdxFlowChart_SoftwareIcons_Expand);
  AProduct.Add('sdxFlowChart_SoftwareIcons_Collapse', @sdxFlowChart_SoftwareIcons_Collapse);
  AProduct.Add('sdxFlowChart_SoftwareIcons_Add', @sdxFlowChart_SoftwareIcons_Add);
  AProduct.Add('sdxFlowChart_SoftwareIcons_Remove', @sdxFlowChart_SoftwareIcons_Remove);
  AProduct.Add('sdxFlowChart_SoftwareIcons_ZoomIn', @sdxFlowChart_SoftwareIcons_ZoomIn);
  AProduct.Add('sdxFlowChart_SoftwareIcons_ZoomOut', @sdxFlowChart_SoftwareIcons_ZoomOut);
  AProduct.Add('sdxFlowChart_SoftwareIcons_Lock', @sdxFlowChart_SoftwareIcons_Lock);
  AProduct.Add('sdxFlowChart_SoftwareIcons_Permission', @sdxFlowChart_SoftwareIcons_Permission);
  AProduct.Add('sdxFlowChart_SoftwareIcons_Sort', @sdxFlowChart_SoftwareIcons_Sort);
  AProduct.Add('sdxFlowChart_SoftwareIcons_Filter', @sdxFlowChart_SoftwareIcons_Filter);
  AProduct.Add('sdxFlowChart_SoftwareIcons_Tools', @sdxFlowChart_SoftwareIcons_Tools);
  AProduct.Add('sdxFlowChart_SoftwareIcons_Properties', @sdxFlowChart_SoftwareIcons_Properties);
  AProduct.Add('sdxFlowChart_SoftwareIcons_Calendar', @sdxFlowChart_SoftwareIcons_Calendar);
  AProduct.Add('sdxFlowChart_SoftwareIcons_Document', @sdxFlowChart_SoftwareIcons_Document);
  AProduct.Add('sdxFlowChart_SoftwareIcons_Database', @sdxFlowChart_SoftwareIcons_Database);
  AProduct.Add('sdxFlowChart_SoftwareIcons_HardDrive', @sdxFlowChart_SoftwareIcons_HardDrive);
  AProduct.Add('sdxFlowChart_SoftwareIcons_Network', @sdxFlowChart_SoftwareIcons_Network);

  AProduct.Add('sdxFlowChart_DecorativeShapesCaption', @sdxFlowChart_DecorativeShapesCaption);
  AProduct.Add('sdxFlowChart_DecorativeShapes_LightningBolt', @sdxFlowChart_DecorativeShapes_LightningBolt);
  AProduct.Add('sdxFlowChart_DecorativeShapes_Moon', @sdxFlowChart_DecorativeShapes_Moon);
  AProduct.Add('sdxFlowChart_DecorativeShapes_Wave', @sdxFlowChart_DecorativeShapes_Wave);
  AProduct.Add('sdxFlowChart_DecorativeShapes_DoubleWave', @sdxFlowChart_DecorativeShapes_DoubleWave);
  AProduct.Add('sdxFlowChart_DecorativeShapes_VerticalScroll', @sdxFlowChart_DecorativeShapes_VerticalScroll);
  AProduct.Add('sdxFlowChart_DecorativeShapes_HorizontalScroll', @sdxFlowChart_DecorativeShapes_HorizontalScroll);
  AProduct.Add('sdxFlowChart_DecorativeShapes_Heart', @sdxFlowChart_DecorativeShapes_Heart);
  AProduct.Add('sdxFlowChart_DecorativeShapes_DownRibbon', @sdxFlowChart_DecorativeShapes_DownRibbon);
  AProduct.Add('sdxFlowChart_DecorativeShapes_UpRibbon', @sdxFlowChart_DecorativeShapes_UpRibbon);
  AProduct.Add('sdxFlowChart_DecorativeShapes_Cloud', @sdxFlowChart_DecorativeShapes_Cloud);

  AProduct.Add('sdxFlowChart_ArrowShapesCaption', @sdxFlowChart_ArrowShapesCaption);
  AProduct.Add('sdxFlowChart_ArrowShapes_SimpleArrow', @sdxFlowChart_ArrowShapes_SimpleArrow);
  AProduct.Add('sdxFlowChart_ArrowShapes_SimpleDoubleArrow', @sdxFlowChart_ArrowShapes_SimpleDoubleArrow);
  AProduct.Add('sdxFlowChart_ArrowShapes_ModernArrow', @sdxFlowChart_ArrowShapes_ModernArrow);
  AProduct.Add('sdxFlowChart_ArrowShapes_FlexibleArrow', @sdxFlowChart_ArrowShapes_FlexibleArrow);
  AProduct.Add('sdxFlowChart_ArrowShapes_BentArrow', @sdxFlowChart_ArrowShapes_BentArrow);
  AProduct.Add('sdxFlowChart_ArrowShapes_UTurnArrow', @sdxFlowChart_ArrowShapes_UTurnArrow);
  AProduct.Add('sdxFlowChart_ArrowShapes_SharpBentArrow', @sdxFlowChart_ArrowShapes_SharpBentArrow);
  AProduct.Add('sdxFlowChart_ArrowShapes_CurvedRightArrow', @sdxFlowChart_ArrowShapes_CurvedRightArrow);
  AProduct.Add('sdxFlowChart_ArrowShapes_CurvedLeftArrow', @sdxFlowChart_ArrowShapes_CurvedLeftArrow);
  AProduct.Add('sdxFlowChart_ArrowShapes_NotchedArrow', @sdxFlowChart_ArrowShapes_NotchedArrow);
  AProduct.Add('sdxFlowChart_ArrowShapes_StripedArrow', @sdxFlowChart_ArrowShapes_StripedArrow);
  AProduct.Add('sdxFlowChart_ArrowShapes_BlockArrow', @sdxFlowChart_ArrowShapes_BlockArrow);
  AProduct.Add('sdxFlowChart_ArrowShapes_CircularArrow', @sdxFlowChart_ArrowShapes_CircularArrow);
  AProduct.Add('sdxFlowChart_ArrowShapes_QuadArrow', @sdxFlowChart_ArrowShapes_QuadArrow);
  AProduct.Add('sdxFlowChart_ArrowShapes_LeftRightUpArrow', @sdxFlowChart_ArrowShapes_LeftRightUpArrow);
  AProduct.Add('sdxFlowChart_ArrowShapes_LeftRightArrowBlock', @sdxFlowChart_ArrowShapes_LeftRightArrowBlock);
  AProduct.Add('sdxFlowChart_ArrowShapes_QuadArrowBlock', @sdxFlowChart_ArrowShapes_QuadArrowBlock);

  AProduct.Add('sdxFlowChart_Arrow_Open90', @sdxFlowChart_Arrow_Open90);
  AProduct.Add('sdxFlowChart_Arrow_Filled90', @sdxFlowChart_Arrow_Filled90);
  AProduct.Add('sdxFlowChart_Arrow_ClosedDot', @sdxFlowChart_Arrow_ClosedDot);
  AProduct.Add('sdxFlowChart_Arrow_FilledDot', @sdxFlowChart_Arrow_FilledDot);
  AProduct.Add('sdxFlowChart_Arrow_OpenFletch', @sdxFlowChart_Arrow_OpenFletch);
  AProduct.Add('sdxFlowChart_Arrow_FilledFletch', @sdxFlowChart_Arrow_FilledFletch);
  AProduct.Add('sdxFlowChart_Arrow_Diamond', @sdxFlowChart_Arrow_Diamond);
  AProduct.Add('sdxFlowChart_Arrow_FilledDiamond', @sdxFlowChart_Arrow_FilledDiamond);
  AProduct.Add('sdxFlowChart_Arrow_ClosedDiamond', @sdxFlowChart_Arrow_ClosedDiamond);
  AProduct.Add('sdxFlowChart_Arrow_IndentedFilledArrow', @sdxFlowChart_Arrow_IndentedFilledArrow);
  AProduct.Add('sdxFlowChart_Arrow_OutdentedFilledArrow', @sdxFlowChart_Arrow_OutdentedFilledArrow);
  AProduct.Add('sdxFlowChart_Arrow_FilledSquare', @sdxFlowChart_Arrow_FilledSquare);
  AProduct.Add('sdxFlowChart_Arrow_ClosedASMEArrow', @sdxFlowChart_Arrow_ClosedASMEArrow);
  AProduct.Add('sdxFlowChart_Arrow_FilledDoubleArrow', @sdxFlowChart_Arrow_FilledDoubleArrow);
  AProduct.Add('sdxFlowChart_Arrow_ClosedDoubleArrow', @sdxFlowChart_Arrow_ClosedDoubleArrow);
end;

procedure AddFlowChartResourceStringNames(AProduct: TdxProductResourceStrings);
begin
  AProduct.Add('sdxFlowChartArrowSizeCustom', @sdxFlowChartArrowSizeCustom);
  AProduct.Add('sdxFlowChartArrowSizeSmall', @sdxFlowChartArrowSizeSmall);
  AProduct.Add('sdxFlowChartArrowSizeMedium', @sdxFlowChartArrowSizeMedium);
  AProduct.Add('sdxFlowChartArrowSizeLarge', @sdxFlowChartArrowSizeLarge);
  AProduct.Add('sdxFlowChartArrowSizeExtraLarge', @sdxFlowChartArrowSizeExtraLarge);
  AProduct.Add('sdxFlowChartArrowSizeHuge', @sdxFlowChartArrowSizeHuge);

  AProduct.Add('sdxFlowChartArrowStyleNone', @sdxFlowChartArrowStyleNone);
  AProduct.Add('sdxFlowChartArrowStyleArrow', @sdxFlowChartArrowStyleArrow);
  AProduct.Add('sdxFlowChartArrowStyleEllipseArrow', @sdxFlowChartArrowStyleEllipseArrow);
  AProduct.Add('sdxFlowChartArrowStyleRectArrow', @sdxFlowChartArrowStyleRectArrow);
  AProduct.Add('sdxFlowChartArrowStyleClosedASMEarrow', @sdxFlowChartArrowStyleClosedASMEarrow);
  AProduct.Add('sdxFlowChartArrowStyleFilledASMEarrow', @sdxFlowChartArrowStyleFilledASMEarrow);
  AProduct.Add('sdxFlowChartArrowStyleClosedArrow', @sdxFlowChartArrowStyleClosedArrow);
  AProduct.Add('sdxFlowChartArrowStyleFilledArrow', @sdxFlowChartArrowStyleFilledArrow);
  AProduct.Add('sdxFlowChartArrowStyleIndentedClosedArrow', @sdxFlowChartArrowStyleIndentedClosedArrow);
  AProduct.Add('sdxFlowChartArrowStyleIndentedFilledArrow', @sdxFlowChartArrowStyleIndentedFilledArrow);
  AProduct.Add('sdxFlowChartArrowStyleOutdentedClosedArrow', @sdxFlowChartArrowStyleOutdentedClosedArrow);
  AProduct.Add('sdxFlowChartArrowStyleOutdentedFilledArrow', @sdxFlowChartArrowStyleOutdentedFilledArrow);
  AProduct.Add('sdxFlowChartArrowStyleClosedDoubleArrow', @sdxFlowChartArrowStyleClosedDoubleArrow);
  AProduct.Add('sdxFlowChartArrowStyleFilledDoubleArrow', @sdxFlowChartArrowStyleFilledDoubleArrow);
  AProduct.Add('sdxFlowChartArrowStyleDiamond', @sdxFlowChartArrowStyleDiamond);
  AProduct.Add('sdxFlowChartArrowStyleFilledDiamond', @sdxFlowChartArrowStyleFilledDiamond);
  AProduct.Add('sdxFlowChartArrowStyleClosedDiamond', @sdxFlowChartArrowStyleClosedDiamond);
  AProduct.Add('sdxFlowChartArrowStyleFilledClosedDiamond', @sdxFlowChartArrowStyleFilledClosedDiamond);
  AProduct.Add('sdxFlowChartArrowStyleDimensionLine', @sdxFlowChartArrowStyleDimensionLine);
  AProduct.Add('sdxFlowChartArrowStyleBackslash', @sdxFlowChartArrowStyleBackslash);
  AProduct.Add('sdxFlowChartArrowStyleOpenOneDash', @sdxFlowChartArrowStyleOpenOneDash);
  AProduct.Add('sdxFlowChartArrowStyleOpenTwoDash', @sdxFlowChartArrowStyleOpenTwoDash);
  AProduct.Add('sdxFlowChartArrowStyleOpenThreeDash', @sdxFlowChartArrowStyleOpenThreeDash);
  AProduct.Add('sdxFlowChartArrowStyleClosedOneDash', @sdxFlowChartArrowStyleClosedOneDash);
  AProduct.Add('sdxFlowChartArrowStyleClosedTwoDash', @sdxFlowChartArrowStyleClosedTwoDash);
  AProduct.Add('sdxFlowChartArrowStyleClosedThreeDash', @sdxFlowChartArrowStyleClosedThreeDash);
  AProduct.Add('sdxFlowChartArrowStyleFilledOneDash', @sdxFlowChartArrowStyleFilledOneDash);
  AProduct.Add('sdxFlowChartArrowStyleFilledTwoDash', @sdxFlowChartArrowStyleFilledTwoDash);
  AProduct.Add('sdxFlowChartArrowStyleFilledThreeDash', @sdxFlowChartArrowStyleFilledThreeDash);

  AProduct.Add('sdxFlowChartConnectionEditorArrowColor', @sdxFlowChartConnectionEditorArrowColor);
  AProduct.Add('sdxFlowChartConnectionEditorArrowSize', @sdxFlowChartConnectionEditorArrowSize);
  AProduct.Add('sdxFlowChartConnectionEditorArrowStyle', @sdxFlowChartConnectionEditorArrowStyle);
  AProduct.Add('sdxFlowChartConnectionEditorCaption', @sdxFlowChartConnectionEditorCaption);
  AProduct.Add('sdxFlowChartConnectionEditorColor', @sdxFlowChartConnectionEditorColor);
  AProduct.Add('sdxFlowChartConnectionEditorDestination', @sdxFlowChartConnectionEditorDestination);
  AProduct.Add('sdxFlowChartConnectionEditorLinkedPoint', @sdxFlowChartConnectionEditorLinkedPoint);
  AProduct.Add('sdxFlowChartConnectionEditorSource', @sdxFlowChartConnectionEditorSource);
  AProduct.Add('sdxFlowChartConnectionEditorText', @sdxFlowChartConnectionEditorText);
  AProduct.Add('sdxFlowChartConnectionEditorTextFontHint', @sdxFlowChartConnectionEditorTextFontHint);

  AProduct.Add('sdxFlowChartDialogButtonOk', @sdxFlowChartDialogButtonOk);
  AProduct.Add('sdxFlowChartDialogButtonCancel', @sdxFlowChartDialogButtonCancel);

  AProduct.Add('sdxFlowChartBorderStyleAdjust', @sdxFlowChartBorderStyleAdjust);
  AProduct.Add('sdxFlowChartBorderStyleBottom', @sdxFlowChartBorderStyleBottom);
  AProduct.Add('sdxFlowChartBorderStyleDiagonal', @sdxFlowChartBorderStyleDiagonal);
  AProduct.Add('sdxFlowChartBorderStyleFlat', @sdxFlowChartBorderStyleFlat);
  AProduct.Add('sdxFlowChartBorderStyleLeft', @sdxFlowChartBorderStyleLeft);
  AProduct.Add('sdxFlowChartBorderStyleMiddle', @sdxFlowChartBorderStyleMiddle);
  AProduct.Add('sdxFlowChartBorderStyleMono', @sdxFlowChartBorderStyleMono);
  AProduct.Add('sdxFlowChartBorderStyleRight', @sdxFlowChartBorderStyleRight);
  AProduct.Add('sdxFlowChartBorderStyleSoft', @sdxFlowChartBorderStyleSoft);
  AProduct.Add('sdxFlowChartBorderStyleTop', @sdxFlowChartBorderStyleTop);

  AProduct.Add('sdxFlowChartEdgeStyleRaisedIn', @sdxFlowChartEdgeStyleRaisedIn);
  AProduct.Add('sdxFlowChartEdgeStyleRaisedOut', @sdxFlowChartEdgeStyleRaisedOut);
  AProduct.Add('sdxFlowChartEdgeStyleSunkenIn', @sdxFlowChartEdgeStyleSunkenIn);
  AProduct.Add('sdxFlowChartEdgeStyleSunkenOut', @sdxFlowChartEdgeStyleSunkenOut);

  AProduct.Add('sdxFlowChartEditorChildItem', @sdxFlowChartEditorChildItem);
  AProduct.Add('sdxFlowChartEditorConnection', @sdxFlowChartEditorConnection);
  AProduct.Add('sdxFlowChartEditorConnectionArrowDestinationHint', @sdxFlowChartEditorConnectionArrowDestinationHint);
  AProduct.Add('sdxFlowChartEditorConnectionArrowDestinationSizeHint', @sdxFlowChartEditorConnectionArrowDestinationSizeHint);
  AProduct.Add('sdxFlowChartEditorConnectionArrowSourceHint', @sdxFlowChartEditorConnectionArrowSourceHint);
  AProduct.Add('sdxFlowChartEditorConnectionArrowSourceSizeHint', @sdxFlowChartEditorConnectionArrowSourceSizeHint);
  AProduct.Add('sdxFlowChartEditorConnectionLinkedPointDestinationHint', @sdxFlowChartEditorConnectionLinkedPointDestinationHint);
  AProduct.Add('sdxFlowChartEditorConnectionLinkedPointSourceHint', @sdxFlowChartEditorConnectionLinkedPointSourceHint);
  AProduct.Add('sdxFlowChartEditorConnectionStyleHint', @sdxFlowChartEditorConnectionStyleHint);
  AProduct.Add('sdxFlowChartEditorConnectionTextFontHint', @sdxFlowChartEditorConnectionTextFontHint);
  AProduct.Add('sdxFlowChartEditorCreate', @sdxFlowChartEditorCreate);
  AProduct.Add('sdxFlowChartEditorCreateConnectionHint', @sdxFlowChartEditorCreateConnectionHint);
  AProduct.Add('sdxFlowChartEditorCreateObjectHint', @sdxFlowChartEditorCreateObjectHint);
  AProduct.Add('sdxFlowChartEditorEdit', @sdxFlowChartEditorEdit);
  AProduct.Add('sdxFlowChartEditorEditBringToFront', @sdxFlowChartEditorEditBringToFront);
  AProduct.Add('sdxFlowChartEditorEditClearSelection', @sdxFlowChartEditorEditClearSelection);
  AProduct.Add('sdxFlowChartEditorEditCopy', @sdxFlowChartEditorEditCopy);
  AProduct.Add('sdxFlowChartEditorEditCut', @sdxFlowChartEditorEditCut);
  AProduct.Add('sdxFlowChartEditorEditDelete', @sdxFlowChartEditorEditDelete);
  AProduct.Add('sdxFlowChartEditorEditPaste', @sdxFlowChartEditorEditPaste);
  AProduct.Add('sdxFlowChartEditorEditSelectAll', @sdxFlowChartEditorEditSelectAll);
  AProduct.Add('sdxFlowChartEditorEditSendToBack', @sdxFlowChartEditorEditSendToBack);
  AProduct.Add('sdxFlowChartEditorEditUndo', @sdxFlowChartEditorEditUndo);
  AProduct.Add('sdxFlowChartEditorFile', @sdxFlowChartEditorFile);
  AProduct.Add('sdxFlowChartEditorFileOpen', @sdxFlowChartEditorFileOpen);
  AProduct.Add('sdxFlowChartEditorFileSave', @sdxFlowChartEditorFileSave);
  AProduct.Add('sdxFlowChartEditorFitHint', @sdxFlowChartEditorFitHint);
  AProduct.Add('sdxFlowChartEditorHelp', @sdxFlowChartEditorHelp);
  AProduct.Add('sdxFlowChartEditorHelpContents', @sdxFlowChartEditorHelpContents);
  AProduct.Add('sdxFlowChartEditorMainItemOfUnion', @sdxFlowChartEditorMainItemOfUnion);
  AProduct.Add('sdxFlowChartEditorObject', @sdxFlowChartEditorObject);
  AProduct.Add('sdxFlowChartEditorObjectImagePositionHint', @sdxFlowChartEditorObjectImagePositionHint);
  AProduct.Add('sdxFlowChartEditorObjectLineWidthHint', @sdxFlowChartEditorObjectLineWidthHint);
  AProduct.Add('sdxFlowChartEditorObjectShapeStyleHint', @sdxFlowChartEditorObjectShapeStyleHint);
  AProduct.Add('sdxFlowChartEditorObjectTextFontHint', @sdxFlowChartEditorObjectTextFontHint);
  AProduct.Add('sdxFlowChartEditorObjectTextPositionHint', @sdxFlowChartEditorObjectTextPositionHint);
  AProduct.Add('sdxFlowChartEditorOptions', @sdxFlowChartEditorOptions);
  AProduct.Add('sdxFlowChartEditorOptionsDynamicMoving', @sdxFlowChartEditorOptionsDynamicMoving);
  AProduct.Add('sdxFlowChartEditorOptionsDynamicSizing', @sdxFlowChartEditorOptionsDynamicSizing);
  AProduct.Add('sdxFlowChartEditorPixels', @sdxFlowChartEditorPixels);
  AProduct.Add('sdxFlowChartEditorPoint', @sdxFlowChartEditorPoint);
  AProduct.Add('sdxFlowChartEditorProperties', @sdxFlowChartEditorProperties);
  AProduct.Add('sdxFlowChartEditorUnions', @sdxFlowChartEditorUnions);
  AProduct.Add('sdxFlowChartEditorUnionsAdd', @sdxFlowChartEditorUnionsAdd);
  AProduct.Add('sdxFlowChartEditorUnionsClear', @sdxFlowChartEditorUnionsClear);
  AProduct.Add('sdxFlowChartEditorUnionsClearAll', @sdxFlowChartEditorUnionsClearAll);
  AProduct.Add('sdxFlowChartEditorUnionsNew', @sdxFlowChartEditorUnionsNew);
  AProduct.Add('sdxFlowChartEditorUnionsRemove', @sdxFlowChartEditorUnionsRemove);
  AProduct.Add('sdxFlowChartEditorView', @sdxFlowChartEditorView);
  AProduct.Add('sdxFlowChartEditorViewAntialiasing', @sdxFlowChartEditorViewAntialiasing);
  AProduct.Add('sdxFlowChartEditorViewActualSize', @sdxFlowChartEditorViewActualSize);
  AProduct.Add('sdxFlowChartEditorViewFit', @sdxFlowChartEditorViewFit);
  AProduct.Add('sdxFlowChartEditorViewZoomIn', @sdxFlowChartEditorViewZoomIn);
  AProduct.Add('sdxFlowChartEditorViewZoomOut', @sdxFlowChartEditorViewZoomOut);
  AProduct.Add('sdxFlowChartEditorZoomHint', @sdxFlowChartEditorZoomHint);

  AProduct.Add('sdxFlowChartObjectEditorBackgroundColor', @sdxFlowChartObjectEditorBackgroundColor);
  AProduct.Add('sdxFlowChartObjectEditorBorderStyle', @sdxFlowChartObjectEditorBorderStyle);
  AProduct.Add('sdxFlowChartObjectEditorCaption', @sdxFlowChartObjectEditorCaption);
  AProduct.Add('sdxFlowChartObjectEditorEdgeStyle', @sdxFlowChartObjectEditorEdgeStyle);
  AProduct.Add('sdxFlowChartObjectEditorFrameTab', @sdxFlowChartObjectEditorFrameTab);
  AProduct.Add('sdxFlowChartObjectEditorGeneralTab', @sdxFlowChartObjectEditorGeneralTab);
  AProduct.Add('sdxFlowChartObjectEditorHeight', @sdxFlowChartObjectEditorHeight);
  AProduct.Add('sdxFlowChartObjectEditorImageClear', @sdxFlowChartObjectEditorImageClear);
  AProduct.Add('sdxFlowChartObjectEditorImageLayout', @sdxFlowChartObjectEditorImageLayout);
  AProduct.Add('sdxFlowChartObjectEditorImageTab', @sdxFlowChartObjectEditorImageTab);
  AProduct.Add('sdxFlowChartObjectEditorLineWidth', @sdxFlowChartObjectEditorLineWidth);
  AProduct.Add('sdxFlowChartObjectEditorShapeColor', @sdxFlowChartObjectEditorShapeColor);
  AProduct.Add('sdxFlowChartObjectEditorShapeType', @sdxFlowChartObjectEditorShapeType);
  AProduct.Add('sdxFlowChartObjectEditorText', @sdxFlowChartObjectEditorText);
  AProduct.Add('sdxFlowChartObjectEditorTextLayout', @sdxFlowChartObjectEditorTextLayout);
  AProduct.Add('sdxFlowChartObjectEditorTransparent', @sdxFlowChartObjectEditorTransparent);
  AProduct.Add('sdxFlowChartObjectEditorWidth', @sdxFlowChartObjectEditorWidth);

  AProduct.Add('sdxFlowChartLayoutTopLeft', @sdxFlowChartLayoutTopLeft);
  AProduct.Add('sdxFlowChartLayoutTop', @sdxFlowChartLayoutTop);
  AProduct.Add('sdxFlowChartLayoutTopRight', @sdxFlowChartLayoutTopRight);
  AProduct.Add('sdxFlowChartLayoutLeft', @sdxFlowChartLayoutLeft);
  AProduct.Add('sdxFlowChartLayoutCenter', @sdxFlowChartLayoutCenter);
  AProduct.Add('sdxFlowChartLayoutRight', @sdxFlowChartLayoutRight);
  AProduct.Add('sdxFlowChartLayoutBottomLeft', @sdxFlowChartLayoutBottomLeft);
  AProduct.Add('sdxFlowChartLayoutBottom', @sdxFlowChartLayoutBottom);
  AProduct.Add('sdxFlowChartLayoutBottomRight', @sdxFlowChartLayoutBottomRight);

  AProduct.Add('sdxFlowChartShapeTypeNone', @sdxFlowChartShapeTypeNone);
  AProduct.Add('sdxFlowChartShapeTypeRect', @sdxFlowChartShapeTypeRect);
  AProduct.Add('sdxFlowChartShapeTypeEllipse', @sdxFlowChartShapeTypeEllipse);
  AProduct.Add('sdxFlowChartShapeTypeRoundRect', @sdxFlowChartShapeTypeRoundRect);
  AProduct.Add('sdxFlowChartShapeTypeDiamond', @sdxFlowChartShapeTypeDiamond);
  AProduct.Add('sdxFlowChartShapeTypeNorthTriangle', @sdxFlowChartShapeTypeNorthTriangle);
  AProduct.Add('sdxFlowChartShapeTypeSouthTriangle', @sdxFlowChartShapeTypeSouthTriangle);
  AProduct.Add('sdxFlowChartShapeTypeEastTriangle', @sdxFlowChartShapeTypeEastTriangle);
  AProduct.Add('sdxFlowChartShapeTypeWestTriangle', @sdxFlowChartShapeTypeWestTriangle);
  AProduct.Add('sdxFlowChartShapeTypeHexagon', @sdxFlowChartShapeTypeHexagon);

  AProduct.Add('sdxFlowChartUnion', @sdxFlowChartUnion);
  AProduct.Add('sdxFlowChartUnions', @sdxFlowChartUnions);
  AProduct.Add('sdxFlowChartUnionEditorCaption', @sdxFlowChartUnionEditorCaption);

  AProduct.Add('sdxFlowChartConnectionStyleStraight', @sdxFlowChartConnectionStyleStraight);
  AProduct.Add('sdxFlowChartConnectionStyleCurved', @sdxFlowChartConnectionStyleCurved);
  AProduct.Add('sdxFlowChartConnectionStyleRectHorizontal', @sdxFlowChartConnectionStyleRectHorizontal);
  AProduct.Add('sdxFlowChartConnectionStyleRectVertical', @sdxFlowChartConnectionStyleRectVertical);

  AddFlowChartVisioResourceStringNames(AProduct);

  AProduct.Add('sdxFlowChartEditorLineSolid', @sdxFlowChartEditorLineSolid);
  AProduct.Add('sdxFlowChartEditorLineDashed', @sdxFlowChartEditorLineDashed);
  AProduct.Add('sdxFlowChartEditorLineDotted', @sdxFlowChartEditorLineDotted);
  AProduct.Add('sdxFlowChartEditorLineDashDotted', @sdxFlowChartEditorLineDashDotted);
  AProduct.Add('sdxFlowChartEditorLineDashDoubleDotted', @sdxFlowChartEditorLineDashDoubleDotted);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressFlowChart', @AddFlowChartResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressFlowChart');
end.
