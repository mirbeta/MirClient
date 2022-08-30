//---------------------------------------------------------------------------
#ifndef StandardRLMainH
#define StandardRLMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Graphics.hpp>
#include <Forms.hpp>
#include <checklst.hpp>
#include <ComCtrls.hpp>
#include <Grids.hpp>
#include <Menus.hpp>
#include <ToolWin.hpp>
#include "dxPSChLbxLnk.hpp"
#include "dxPSCore.hpp"
#include "dxPSGrLnks.hpp"
#include "dxPSLbxLnk.hpp"
#include "dxPSStdGrLnk.hpp"
#include <ExtCtrls.hpp>
#include "dxPSRELnk.hpp"
#include "dxPSTVLnk.hpp"
#include "dxBkgnd.hpp"
#include "dxPrnDev.hpp"
#include "dxPrnPg.hpp"
#include "dxPSBaseGridLnk.hpp"
#include "dxPSCompsProvider.hpp"
#include "dxPSEdgePatterns.hpp"
#include "dxPSEngn.hpp"
#include "dxPSFillPatterns.hpp"
#include "dxPSGlbl.hpp"
#include "dxPSUtl.hpp"
#include "dxWrap.hpp"
#include <CheckLst.hpp>
#include <ImgList.hpp>
#include "cxDrawTextUtils.hpp"
#include "dxPScxEditorProducers.hpp"
#include "dxPScxExtEditorProducers.hpp"
#include "dxPScxPageControlProducer.hpp"
#include "dxPSPDFExport.hpp"
#include "dxPSPDFExportCore.hpp"
#include "dxPSPrVwStd.hpp"
#include "DemoBasicMain.h"
#include <ActnList.hpp>
#include "cxGraphics.hpp"
//---------------------------------------------------------------------------

#undef HDC

class TStandardRLMainForm : public TDemoBasicMainForm
{
__published:  // IDE-managed Components
    TImageList *ilFlags;
    TImageList *ilFontImages;
    TPageControl *PageControl;
    TTabSheet *tsStringGrid;
    TStringGrid *StringGrid;
    TTabSheet *tsDrawGrid;
    TDrawGrid *DrawGrid;
    TTabSheet *tsListBox;
    TListBox *FontsList;
    TCheckListBox *CountryCodeList;
    TTabSheet *tsTreeView;
    TPanel *Panel1;
    TLabel *Label1;
    TComboBox *cbDrives;
    TButton *Button1;
    TButton *Button2;
    TPanel *Panel2;
    TTreeView *TreeView;
    TTabSheet *tsRichEdit;
    TRichEdit *Editor;
    void __fastcall DrawGridDrawCell(TObject *Sender, int Col, int Row,
          TRect &Rect, TGridDrawState State);
    void __fastcall FormCreate(TObject *Sender);
    void __fastcall FormDestroy(TObject *Sender);
    void __fastcall PageControlChange(TObject *Sender);
    void __fastcall FontsListDrawItem(TWinControl *Control, int Index,
          TRect &Rect, TOwnerDrawState State);

    void __fastcall dxComponentPrinterLink2CustomDrawCell(
          TBasedxReportLink *Sender, int ACol, int ARow, TCanvas *ACanvas,
          TRect &ABoundsRect, TRect &AClientRect);
    void __fastcall Button1Click(TObject *Sender);
    void __fastcall Button2Click(TObject *Sender);
    void __fastcall TreeViewExpanded(TObject *Sender, TTreeNode *Node);
    void __fastcall TreeViewCollapsed(TObject *Sender, TTreeNode *Node);
    void __fastcall cbDrivesChange(TObject *Sender);
    void __fastcall TreeViewExpanding(TObject *Sender, TTreeNode *Node,
          bool &AllowExpansion);
private:  // User declarations
    void __fastcall DrawFlag(int Row, int Col, TRect &Rect, TCanvas *ACanvas);
    void __fastcall BuildDriverList();
    String __fastcall GetNodeFullPath(TTreeNode *Node);
    void __fastcall BuildTree(String APath, TTreeNode *AItem);
public:   // User declarations
    Graphics::TBitmap *TtfBitmap,*DevBitmap,*SysBitmap;
    __fastcall TStandardRLMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TStandardRLMainForm *StandardRLMainForm;
//---------------------------------------------------------------------------
#endif
