//---------------------------------------------------------------------------
#ifndef AlphaBlendingMainH
#define AlphaBlendingMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "dxNavBar.hpp"
#include "dxNavBarBase.hpp"
#include "dxNavBarCollns.hpp"
#include "dxNavBarStyles.hpp"
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ExtCtrls.hpp>
#include <ExtDlgs.hpp>
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TfmAlphaBlendingMain : public TForm
{
__published:	// IDE-managed Components
        TLabel *Label9;
        TdxNavBar *nbMain;
        TdxNavBarGroup *gbDocs;
        TdxNavBarGroup *bgPlaces;
        TdxNavBarGroup *bgDetails;
        TdxNavBarItem *biMyMusic;
        TdxNavBarItem *biMyPictures;
        TdxNavBarItem *biReceivedFiles;
        TdxNavBarItem *biDesktop;
        TdxNavBarItem *biMyComputer;
        TdxNavBarItem *biNetwork;
        TdxNavBarStyleItem *stBackground;
        TdxNavBarStyleItem *stGroup1Background;
        TdxNavBarStyleItem *stGroup2Background;
        TdxNavBarStyleItem *stGroup3Background;
        TPanel *Panel5;
        TGroupBox *GroupBox4;
        TLabel *Label7;
        TLabel *lbBgGroup3;
        TTrackBar *tbBgGroup3;
        TPanel *Panel4;
        TImage *iBgGroup3;
        TGroupBox *GroupBox3;
        TLabel *Label5;
        TLabel *lbBgGroup2;
        TTrackBar *tbBgGroup2;
        TPanel *Panel3;
        TImage *iBgGroup2;
        TGroupBox *GroupBox2;
        TLabel *Label3;
        TLabel *lbBgGroup1;
        TTrackBar *tbBgGroup1;
        TPanel *Panel2;
        TImage *iBgGroup1;
        TGroupBox *GroupBox1;
        TLabel *Label1;
        TLabel *lbBg;
        TPanel *Panel1;
        TImage *iBg;
        TTrackBar *tbBg;
        TImageList *ilSmall;
        TImageList *ilLarge;
        TOpenPictureDialog *OpenPictureDialog1;
        void __fastcall lbBgDblClick(TObject *Sender);
        void __fastcall lbBgGroup1DblClick(TObject *Sender);
        void __fastcall lbBgGroup2DblClick(TObject *Sender);
        void __fastcall lbBgGroup3DblClick(TObject *Sender);
        void __fastcall iBgClick(TObject *Sender);
        void __fastcall iBgGroup1Click(TObject *Sender);
        void __fastcall iBgGroup2Click(TObject *Sender);
        void __fastcall iBgGroup3Click(TObject *Sender);
        void __fastcall tbBgChange(TObject *Sender);
        void __fastcall tbBgGroup1Change(TObject *Sender);
        void __fastcall tbBgGroup2Change(TObject *Sender);
        void __fastcall tbBgGroup3Change(TObject *Sender);
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall nbMainGroupHotTrack(TObject *Sender,
          TdxNavBarGroup *AGroup);
        void __fastcall nbMainCalcGroupClientHeight(TObject *Sender,
          TdxNavBarGroupViewInfo *AViewInfo, int &AHeight);
        void __fastcall nbMainCustomDrawGroupClientForeground(
          TObject *Sender, TCanvas *ACanvas,
          TdxNavBarGroupViewInfo *AViewInfo, bool &AHandled);
        void __fastcall nbMainGetGroupHint(TObject *Sender,
          TdxNavBarGroup *AGroup, String &AHint);
        void __fastcall nbMainCalcGroupHintRect(TObject *Sender,
          TdxNavBarGroup *AGroup, TdxNavBarViewInfo *AViewInfo, TRect &R);
        void __fastcall nbMainCustomDrawGroupHint(TObject *Sender,
          TCanvas *ACanvas, TdxNavBarGroup *AGroup, String AHint,
          TRect &R, bool &AHandled);
private:	// User declarations
        void ClearBgImage(TdxNavBarStyleItem *AStyleItem);
        void SetBgImage(TdxNavBarStyleItem *AStyleItem);
        void SetBgAlphaBlending(TdxNavBarStyleItem *AStyleItem, Byte AValue);
public:		// User declarations
        __fastcall TfmAlphaBlendingMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfmAlphaBlendingMain *fmAlphaBlendingMain;
//---------------------------------------------------------------------------
#endif
