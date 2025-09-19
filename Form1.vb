Imports System.IO


Public Class Form1
    Public Declare Sub Sleep Lib "Kernel32.dll" (ByVal dwMillisenconds As Long)
#Region "variables"
#Region "Structure"
    Structure sPointIndex 'coordonées des point de la bride out et le numero de la CH 
        Dim MypointMoovingClose As Point 'point mooving 
        Dim MyPointFixeclose As Point 'point fixe
        Dim ObjNumberFixe As Int16       'numero de l'obj fixe
        Dim ObjNumberMooving As Int16   'numero de l obj en mouvement
        Dim LinkNumberFixe As Int16     'numero de la liaison de l'objet fixe
        Dim LinkNumberMooving As Int16  'numero de la liaison de l objet mooving
        Dim Find As Boolean             'j'ai trouver une solution
        Dim StickLocation As Point        'le point haut a droite ou doit ce coller  le moovingObj par le point haut a gauche
        Dim ObjNameParent As String         '' nom de l'obj fixe coller a l'obj
        Dim objNameChild As String          '' nom de l'obj mooving
        Dim Node As String                  '' nom du node qui relie deux obj
    End Structure
    Structure sSegmentSize 'information sur la taille des segments entre les obj
        Dim size As Double
        Dim ObjMooving As Int16
        Dim PointMooving As Point
        Dim ObjFixe As Int16
        Dim PointFixe As Point

    End Structure
#End Region
    '' Enum ePosition
    '' eIn = 0
    '' eOut = 1
    ''  eTop = 2
    ''  eBot = 3

    '' End Enum




    Private PathDataSave As String 'chemin de sauvegarde 
    Private MyNearlyCH As sPointIndex
    ' Drawing parameters.
    Private m_GridX As Integer = 100
    Private m_GridY As Integer = 100
    ''  Private m_PageWid As Integer = 400
    '' Private m_PageHgt As Integer = 300
    '' Private m_ShadowThickness As Integer = 1
    Private off As Point
    ' Pens.
    Private m_PenOldLine As Pen = Pens.Crimson


    ' Options.
    Private m_ShowGrid As Boolean = False
    Private m_SnapToGrid As Boolean = False
    'objet CH ....
    Private myCtrlCVxx(100) As CtrlCVxx
    Private MyAllCtrlCVxx As CtrlCVxx
    Private MyCtrlLabel(200) As CtrlLabel
    Private IndexCtrlCV As Integer  'nombre de CH 


    ' For drawing a new line.
    Private m_Drawing As Boolean = False
    Private m_X1 As Single
    Private m_Y1 As Single
    Private m_X2 As Single
    Private m_Y2 As Single

    ' For showing position on the rulers.
    Private m_MouseX As Integer = -10
    Private m_MouseY As Integer = -10

    ' Existing lines.
    Private m_Points1() As PointF = {}
    Private m_Points2() As PointF = {}
    Private PointMilieuLink(10, 10) As Point 'pour tout les linkPoint de tout les obj
    Private linkPointMoovingObj(10) As Point '
    '' Private pointMilieuIN(100) As Point
    '' Private pointMilieuOut(100) As Point
    ''  Private pointMilieuTop(100) As Point
    ''  Private pointMilieuBot(100) As Point
    Private LongeurSegment(5, 5) As Double 'longeur du segment entre la brideout de la chambre posée et la brideIn de la chambre selectionnée.
    Private CHClose As sPointIndex
    ''Private MySegments(10000) As sSegmentSize
    Private pointClose As Point
    Private IndexCHClose As Int16
    'parametre vacuum

    Private CVClicked As Int16 = 0
    Private mydata(10, 50) As String
    Private ObjSelection As CtrlCVxx.eTypObj
    Private indexFamily(10) As Int16
    Private Const GND As Int16 = 0
    Private Const R0 As Int16 = 0
    Private ObjCopied As Int16
    Private lastvalue As Int16
    Private ObjDeleted As Int16 = 0
    Private objName(100) As String     '' noms des objet extrait du fichier Out.txt
    Private Pressure(100) As Double '' valeur associees aux obj extraites du fichier out.txt
    Private NbrLabelPressure As Integer '' nombre de labelpressure
    Private FileVacSimsLoad As String
    Private Const PenningPumpingSpeed As Double = 0.1 ' l/s
    Private Mypulse As CtrlCVxx.sPulse
    Private VariablesTran(,) As Double 'array pour les variables avec transien
    Dim NoVariablesOP, NoVariablesTran, NoPointsOP, NoPointsTran As Int16
    Private VacSpicePath As String = "C:\Spice\bin\"
#End Region

#Region "load Form"
    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        ' m_ShowGrid = mnuOptionsShowGrid.Checked
        m_SnapToGrid = mnuOptionsSnapToGrid.Checked
        'handler 
        AddHandler MyAllCtrlCVxx.ObjClicked, AddressOf InfoObjClicked
        AddHandler MyAllCtrlCVxx.ObjMooving, AddressOf CloserCH
        AddHandler MyAllCtrlCVxx.ButtonRealease, AddressOf StickCH
        AddHandler MyAllCtrlCVxx.ObjRemoved, AddressOf RemovedObj
        AddHandler MyAllCtrlCVxx.ObjCopy, AddressOf IndexObjCopied
        ''  AddHandler MyAllCtrlCVxx.myMouseLeave, AddressOf cacheObjInfo
        ' Arrange the controls.
        Dim wid As Integer = Me.ClientSize.Width '' - picLeftRuler.Left - picLeftRuler.Width - 3
        Dim hgt As Integer = Me.ClientSize.Height '' - picTopRuler.Top - picTopRuler.Height - 3


    End Sub
#End Region

#Region "buttons"
    Private Sub mnuFileExit_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles mnuFileExit.Click
        Me.Close()
    End Sub
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        'to add some new CH
        '' ObjSelection = CtrlCVxx.eFamily.CH
        AddObj(CtrlCVxx.eTypObj.CH, CtrlCVxx.eFamily.CH)



    End Sub
    Private Sub Button2_Click_1(sender As Object, e As EventArgs) Handles Button2.Click
        '' ObjSelection = CtrlCVxx.CtrlCVxx.eFamily.NODE
        AddObj(CtrlCVxx.eTypObj.Node, CtrlCVxx.eFamily.NODE)


    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        '' ObjSelection = CtrlCVxx.eFamily.IP

        AddObj(CtrlCVxx.eTypObj.IP, CtrlCVxx.eFamily.IP)


    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        '' ObjSelection = CtrlCVxx.eFamily.START
        AddObj(CtrlCVxx.eTypObj.Start, CtrlCVxx.eFamily.START)

    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        ''ObjSelection = CtrlCVxx.eFamily.END_
        AddObj(CtrlCVxx.eTypObj.End_, CtrlCVxx.eFamily.END_)

    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        '' ObjSelection = CtrlCVxx.eFamily.PEN
        AddObj(CtrlCVxx.eTypObj.PEN, CtrlCVxx.eFamily.PEN)

    End Sub
    Private Sub Button8_Click(sender As Object, e As EventArgs) Handles Button8.Click
        ''ObjSelection = CtrlCVxx.eFamily.GAZ
        AddObj(CtrlCVxx.eTypObj.GAZ, CtrlCVxx.eFamily.GAZ)

    End Sub

    Private Sub Button9_Click(sender As Object, e As EventArgs) Handles Button9.Click
        ''ObjSelection = CtrlCVxx.eFamily.GAZ
        AddObj(CtrlCVxx.eTypObj.Volume1, CtrlCVxx.eFamily.VOLUME)
    End Sub

    Private Sub Button3_MouseHover(sender As Object, e As EventArgs) Handles Button3.MouseHover

        'charge famille IP
        ObjSelection = CtrlCVxx.eFamily.IP
        'charge les image des IP 
        ButtonNorth.Visible = True
        ButtonEast.Visible = True
        ButtonSouth.Visible = True
        ButtonWest.Visible = True
        ButtonEveryWhere.Visible = True

        ButtonWest.Image = My.Resources.IPw
        ButtonNorth.Image = My.Resources.IP
        ButtonEast.Image = My.Resources.IPe
        ButtonSouth.Image = My.Resources.IPs
        ButtonEveryWhere.Image = My.Resources.IPx

    End Sub

    Private Sub Button1_MouseHover(sender As Object, e As EventArgs) Handles Button1.MouseHover

        'charge famille ch
        ObjSelection = CtrlCVxx.eFamily.CH
        'charge les image des CH
        ButtonNorth.Visible = True
        ButtonEast.Visible = False
        ButtonSouth.Visible = True
        ButtonWest.Visible = False
        ButtonEveryWhere.Visible = True

        ButtonNorth.Image = My.Resources.CHph
        ButtonEast.Image = Nothing
        ButtonSouth.Image = My.Resources.CHpb
        ButtonWest.Image = Nothing
        ButtonEveryWhere.Image = My.Resources.CHphb

    End Sub

    Private Sub Button2_MouseHover(sender As Object, e As EventArgs) Handles Button2.MouseHover

        'charge famille node
        ObjSelection = CtrlCVxx.eFamily.NODE
        'charge les image des node
        ButtonNorth.Visible = True
        ButtonEast.Visible = True
        ButtonSouth.Visible = True
        ButtonWest.Visible = True
        ButtonEveryWhere.Visible = True

        ButtonNorth.Image = My.Resources.NodeNE
        ButtonEast.Image = My.Resources.NodeSE
        ButtonSouth.Image = My.Resources.NodeWS
        ButtonWest.Image = My.Resources.NodeNW
        ButtonEveryWhere.Image = My.Resources.nodesize
    End Sub

    Private Sub Button6_MouseHover(sender As Object, e As EventArgs) Handles Button6.MouseHover


        'charge famille penning
        ObjSelection = CtrlCVxx.eFamily.PEN
        'charge les image des penning
        ButtonNorth.Visible = True
        ButtonEast.Visible = True
        ButtonSouth.Visible = True
        ButtonWest.Visible = True
        ButtonEveryWhere.Visible = False

        ButtonNorth.Image = My.Resources.penning
        ButtonEast.Image = My.Resources.penningE
        ButtonSouth.Image = My.Resources.penningS
        ButtonWest.Image = My.Resources.penningW
        ButtonEveryWhere.Image = Nothing
    End Sub

    Private Sub ButtonWest_Click(sender As Object, e As EventArgs) Handles ButtonWest.Click
        Select Case ObjSelection
            Case CtrlCVxx.eFamily.CH
                AddObj(CtrlCVxx.eTypObj.CH, CtrlCVxx.eFamily.CH)

            Case CtrlCVxx.eFamily.IP
                AddObj(CtrlCVxx.eTypObj.IPw, CtrlCVxx.eFamily.IP)

            Case CtrlCVxx.eFamily.NODE
                AddObj(CtrlCVxx.eTypObj.NodeNW, CtrlCVxx.eFamily.NODE)

            Case CtrlCVxx.eFamily.PEN
                AddObj(CtrlCVxx.eTypObj.PENw, CtrlCVxx.eFamily.PEN)



        End Select
    End Sub

#End Region


#Region "function et sub"
    Private Sub AfficheMSG(ByVal message As String)
        ListBox1.Items.Add(Now & vbTab & message)
        ListBox1.SelectedIndex = ListBox1.Items.Count - 1
    End Sub
    Private Sub SnapToGrid(ByRef X As Integer, ByRef Y As Integer)
        ' If grid snap is off, do nothing.
        If Not m_SnapToGrid Then Exit Sub

        Dim ix As Integer = CInt(X / m_GridX)
        Dim iy As Integer = CInt(Y / m_GridY)
        X = ix * m_GridX
        Y = iy * m_GridY
    End Sub


    Private Sub ShowMousePosition(ByVal X As Integer, ByVal Y As Integer)
        m_MouseX = X
        m_MouseY = Y

        '' picLeftRuler.Invalidate()
    End Sub

    Private Function findPointsToDrawline2(ByVal indexObjMooving As Int16) As sPointIndex
        Dim IndexObjFixe As Int16 = 0
        Dim temp As Int16 = 10000
        Dim IndexPointFixe As Int16 = 0
        Dim IndexPointMooving As Int16
        Dim indexSegment As Int16 = 0
        Dim MySegments(1000) As sSegmentSize
        'cherche la brideOut la plus proche 


        'calcule la longeur de tout les segments entre l'obj mooving et le(s) obj posé

        Do
            If myCtrlCVxx(indexObjMooving).LinkPoint(IndexPointMooving)._Enable Then
                For IndexObjFixe = 0 To IndexCtrlCV - 1 'pour chaque obj fixe
                    If IndexObjFixe <> indexObjMooving And myCtrlCVxx(IndexObjFixe).Enabled Then 'sauf l'obj mooving
                        Do  'chaque point de l'obj fixe 
                            If myCtrlCVxx(IndexObjFixe).Enabled And myCtrlCVxx(IndexObjFixe).LinkPoint(IndexPointFixe)._Enable And Not myCtrlCVxx(IndexObjFixe).LinkPoint(IndexPointFixe)._connected Then 'si le point est actif et pas  connecté 
                                'calcule la longeur du segment
                                MySegments(indexSegment).size = Math.Sqrt((myCtrlCVxx(indexObjMooving).LinkPoint(IndexPointMooving)._Point.X - myCtrlCVxx(IndexObjFixe).LinkPoint(IndexPointFixe)._Point.X) ^ 2 + (myCtrlCVxx(indexObjMooving).LinkPoint(IndexPointMooving)._Point.Y - myCtrlCVxx(IndexObjFixe).LinkPoint(IndexPointFixe)._Point.Y) ^ 2)
                                MySegments(indexSegment).PointMooving = myCtrlCVxx(indexObjMooving).LinkPoint(IndexPointMooving)._Point
                                MySegments(indexSegment).ObjMooving = indexObjMooving
                                MySegments(indexSegment).PointFixe = myCtrlCVxx(IndexObjFixe).LinkPoint(IndexPointFixe)._Point
                                MySegments(indexSegment).ObjFixe = IndexObjFixe
                                If myCtrlCVxx(indexObjMooving).LinkPoint(IndexPointMooving)._connected Then
                                    myCtrlCVxx(indexObjMooving).LinkPoint(IndexPointMooving)._connected = False
                                    myCtrlCVxx(myCtrlCVxx(indexObjMooving).LinkPoint(IndexPointMooving)._linkedToObj).LinkPoint(myCtrlCVxx(indexObjMooving).LinkPoint(IndexPointMooving)._linkedToPoint)._connected = False

                                End If

                                ''myCtrlCVxx(IndexObjFixe).LinkPoint(indexObjMooving)._connected = False
                                If MySegments(indexSegment).size < temp Then 'je cherche le plus court ....et prepare la relation entre les obj fixe et mooving
                                    temp = MySegments(indexSegment).size
                                    findPointsToDrawline2.ObjNumberFixe = IndexObjFixe
                                    findPointsToDrawline2.LinkNumberFixe = IndexPointFixe
                                    findPointsToDrawline2.LinkNumberMooving = IndexPointMooving
                                    findPointsToDrawline2.ObjNumberMooving = indexObjMooving
                                    findPointsToDrawline2.MyPointFixeclose = myCtrlCVxx(IndexObjFixe).LinkPoint(IndexPointFixe)._Point 'point sur l'obj fixe d'ou part la linge
                                    findPointsToDrawline2.MypointMoovingClose = myCtrlCVxx(indexObjMooving).LinkPoint(IndexPointMooving)._Point 'point sur
                                    findPointsToDrawline2.ObjNameParent = myCtrlCVxx(IndexObjFixe).CvName
                                    findPointsToDrawline2.objNameChild = myCtrlCVxx(indexObjMooving).CvName

                                    Select Case IndexPointFixe 'determine sur quel face doit venir se coller l'obj mooving

                                        Case 0 'ín
                                            findPointsToDrawline2.StickLocation.X = myCtrlCVxx(IndexObjFixe).LinkPoint(IndexPointFixe)._Point.X - myCtrlCVxx(indexObjMooving).Width '
                                            findPointsToDrawline2.StickLocation.Y = myCtrlCVxx(IndexObjFixe).LinkPoint(IndexPointFixe)._Point.Y - (myCtrlCVxx(indexObjMooving).Height / 2)
                                        Case 1 'out 
                                            findPointsToDrawline2.StickLocation.X = myCtrlCVxx(IndexObjFixe).LinkPoint(IndexPointFixe)._Point.X
                                            findPointsToDrawline2.StickLocation.Y = myCtrlCVxx(IndexObjFixe).LinkPoint(IndexPointFixe)._Point.Y - (myCtrlCVxx(indexObjMooving).Height / 2)
                                        Case 2 'top

                                            findPointsToDrawline2.StickLocation.X = myCtrlCVxx(IndexObjFixe).LinkPoint(IndexPointFixe)._Point.X - (myCtrlCVxx(indexObjMooving).Width / 2)
                                            findPointsToDrawline2.StickLocation.Y = myCtrlCVxx(IndexObjFixe).LinkPoint(IndexPointFixe)._Point.Y - myCtrlCVxx(indexObjMooving).Height

                                        Case 3 'bot
                                            findPointsToDrawline2.StickLocation.X = myCtrlCVxx(IndexObjFixe).LinkPoint(IndexPointFixe)._Point.X - (myCtrlCVxx(indexObjMooving).Width / 2)
                                            findPointsToDrawline2.StickLocation.Y = myCtrlCVxx(IndexObjFixe).LinkPoint(IndexPointFixe)._Point.Y
                                    End Select


                                    If temp < 100 Then 'si il suffisemment pret
                                        findPointsToDrawline2.Find = True

                                    Else
                                        findPointsToDrawline2.Find = False
                                    End If

                                End If
                                indexSegment = indexSegment + 1
                            End If
                            IndexPointFixe = IndexPointFixe + 1
                        Loop Until IndexPointFixe = myCtrlCVxx(IndexObjFixe).LinkNumber  ''  Next
                        IndexPointFixe = 0
                    Else
                        If myCtrlCVxx(indexObjMooving).LinkPoint(IndexPointMooving)._connected Then '' pour suprimer une connection entre obj
                            myCtrlCVxx(indexObjMooving).LinkPoint(IndexPointMooving)._connected = False
                            myCtrlCVxx(myCtrlCVxx(indexObjMooving).LinkPoint(IndexPointMooving)._linkedToObj).LinkPoint(myCtrlCVxx(indexObjMooving).LinkPoint(IndexPointMooving)._linkedToPoint)._connected = False
                            myCtrlCVxx(indexObjMooving).LinkPoint(IndexPointMooving)._LinkedToNode = "nothing"
                            myCtrlCVxx(IndexObjFixe).LinkPoint(IndexPointFixe)._LinkedToNode = "nothing"
                        End If

                    End If
                Next
            End If
            ''Next
            IndexPointMooving = IndexPointMooving + 1
        Loop Until IndexPointMooving = myCtrlCVxx(indexObjMooving).LinkNumber
        IndexPointMooving = 0

        Panel1.Invalidate() 'efface les squelettes de lignes....


    End Function

    Private Sub AddObj(ByVal tyObj As CtrlCVxx.eTypObj, ByVal Family As CtrlCVxx.eFamily)
        'add obj by default CH


        myCtrlCVxx(IndexCtrlCV) = New CtrlCVxx

        myCtrlCVxx(IndexCtrlCV).Index = IndexCtrlCV  'assignation du numero d'index a l'obj.
        indexFamily(Family) = indexFamily(Family) + 1 'ajoute une unité dans la famille concernée
        myCtrlCVxx(IndexCtrlCV).IndexFamily = indexFamily(Family)
        myCtrlCVxx(IndexCtrlCV).Family = Family
        ''myCtrlCVxx(IndexCtrlCV).Regime = CtrlCVxx.eRegime.moleculaire 'definition du regime 
        ''myCtrlCVxx(IndexCtrlCV).Location = mypos
        '' myCtrlCVxx(IndexCtrlCV).longeur = 300  'longeur de la chambre en mm
        '' myCtrlCVxx(IndexCtrlCV).Diametre = 15    'diametre de la chambre en mm
        myCtrlCVxx(IndexCtrlCV).TypeObj = tyObj '' CtrlCVxx.eTypObj.CH 'definition du type de l'objet par defaut CH
        '' myCtrlCVxx(IndexCtrlCV).OutgasingFactor = 0.0000000003
        ''  myCtrlCVxx(IndexCtrlCV).OutgasingType = CtrlCVxx.eBaked.Unbaked
        ''  myCtrlCVxx(IndexCtrlCV).IPpumpingSpeed = 300
        ''  myCtrlCVxx(IndexCtrlCV).DiametreIP = TextBoxIPdiametre.Text
        Panel1.Controls.Add(myCtrlCVxx(IndexCtrlCV))
        IndexCtrlCV = IndexCtrlCV + 1
    End Sub
    Private Function incIndex() As Int16
        lastvalue = lastvalue + 1

        Return lastvalue
    End Function
    Private Sub rebuildDraw()
        Dim objNbr, j, k As Int16
        Dim pointTempo As New Point(4, 4)
        Dim mycondutance As CtrlCVxx.sVacuumModel
        Panel1.Controls.Clear()

        For objNbr = 0 To mydata.GetLength(0) - 1

            lastvalue = 0
            myCtrlCVxx(objNbr) = New CtrlCVxx
            ''myCtrlCVxx(objNbr).Enabled = True
            myCtrlCVxx(objNbr).Index = mydata(objNbr, 0)  'assignation du numero d'index a l'obj.
            myCtrlCVxx(objNbr).CvName = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).IndexFamily = mydata(objNbr, incIndex)

            myCtrlCVxx(objNbr).TypeObj = mydata(objNbr, incIndex)
            pointTempo.X = mydata(objNbr, incIndex)
            pointTempo.Y = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).Location = pointTempo
            myCtrlCVxx(objNbr).longeur = mydata(objNbr, incIndex)

            myCtrlCVxx(objNbr).X_in = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).X_out = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).Y_up = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).Y_down = mydata(objNbr, incIndex)

            myCtrlCVxx(objNbr).Diametre = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).Family = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).OutgasingType = mydata(objNbr, incIndex)
            mycondutance.Conductance = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).Conductance = mycondutance
            indexFamily(myCtrlCVxx(objNbr).Family) = myCtrlCVxx(objNbr).IndexFamily

            k = incIndex()
            'linkpoint 0
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._connected = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._Enable = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._linkedToObj = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._linkedToObjName = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._linkedToPoint = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._LinkedToNode = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._Point.X = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._Point.Y = mydata(objNbr, incIndex)
            k = incIndex()
            'linkpoint 1
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._connected = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._Enable = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._linkedToObj = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._linkedToObjName = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._linkedToPoint = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._LinkedToNode = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._Point.X = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._Point.Y = mydata(objNbr, incIndex)
            k = incIndex()
            'linkpoint 2
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._connected = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._Enable = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._linkedToObj = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._linkedToObjName = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._linkedToPoint = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._LinkedToNode = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._Point.X = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._Point.Y = mydata(objNbr, incIndex)
            k = incIndex()
            'linkpoint 3
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._connected = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._Enable = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._linkedToObj = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._linkedToObjName = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._linkedToPoint = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._LinkedToNode = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._Point.X = mydata(objNbr, incIndex)
            myCtrlCVxx(objNbr).LinkPoint(mydata(objNbr, k))._Point.Y = mydata(objNbr, incIndex)

            Panel1.Controls.Add(myCtrlCVxx(objNbr))
            InfoObjClicked(objNbr)
        Next
        IndexCtrlCV = objNbr
    End Sub

    Private Sub ConverToSpiceArray()
        Dim i, j As Integer
        Dim obj As CtrlCVxx

        Me.PathDataSave = "E:\Spice\bin\spicemodel.sp" '' SaveFileDialog1.FileName


        ' If Me.SaveFileDialog1.FileName <> Nothing Then
        File.Delete(PathDataSave)
        FileOpen(1, PathDataSave, OpenMode.Append)



        PrintLine(1, "VacuumGroupInst")
        PrintLine(1, "*SPICE Netlist generated by VacInstGroup on " & Now & " " & SaveFileDialog1.FileName)
        For i = 0 To IndexCtrlCV - 1
            obj = myCtrlCVxx(i)
            Select Case obj.TypeObj

                Case CtrlCVxx.eTypObj.CH, CtrlCVxx.eTypObj.CHpb, CtrlCVxx.eTypObj.CHph, CtrlCVxx.eTypObj.CHphb, CtrlCVxx.eTypObj.Volume1
                    If obj.Enabled Then

                        PrintLine(1, "***********************" & obj.CvName & "**********************************")
                        PrintLine(1, "*Schematic Netlist:")
                        PrintLine(1, "* Name :" & obj.CvName)
                        PrintLine(1, "* lenght(cm) :" & obj.longeur)
                        PrintLine(1, "* Diametre(cm) :" & obj.Diametre)
                        If obj.TypeObj = CtrlCVxx.eTypObj.Volume1 Then PrintLine(1, "* height(cm) :" & obj.volumey & "  width(cm) :" & obj.volumez)


                        For j = 0 To obj.SegmentNbr - 1
                            If obj.NegActive Then PrintLine(1, "RNEG_" & obj.CvNameArray(i) & " " & GND & " " & "middle_" & obj.CvNameArray(i) & " {1/" & obj.NegPumpingSpeedArray(i) & "}") '' si ya du neg
                            PrintLine(1, "* Segment :" & i & "/" & obj.SegmentNbr)
                            PrintLine(1, "IQ_" & obj.CvNameArray(i) & " " & GND & " " & "middle_" & obj.CvNameArray(i) & " " & obj.OutGasingArray(i)) '' degasage
                            PrintLine(1, "Rup_" & obj.CvNameArray(i) & " " & obj.LinkPoint(0)._LinkedToNode & " " & "middle_" & obj.CvNameArray(i) & " {1/ " & obj.ConductanceArray(i).C_in & "}") '' demi chambre left
                            PrintLine(1, "Rdown_" & obj.CvNameArray(i) & " " & "middle_" & obj.CvNameArray(i) & " " & obj.LinkPoint(1)._LinkedToNode & " {1/" & obj.ConductanceArray(i).C_out & "}") '' demi chambre right
                            PrintLine(1, "C_volume_" & obj.CvNameArray(i) & " " & GND & " " & "middle_" & obj.CvNameArray(i) & " " & obj.VolumeArray(i)) '' volume
                        Next
                        If obj.LinkPoint(2)._Enable And obj.LinkPoint(2)._connected Then PrintLine(1, "Rtop_" & obj.CvName & " " & "middle_" & obj.CvName & " " & obj.LinkPoint(2)._LinkedToNode & " " & R0) '' link point top
                        If obj.LinkPoint(3)._Enable And obj.LinkPoint(3)._connected Then PrintLine(1, "Rbot_" & obj.CvName & " " & "middle_" & obj.CvName & " " & obj.LinkPoint(3)._LinkedToNode & " " & R0) '' link point bot
                    End If


                Case CtrlCVxx.eTypObj.IP, CtrlCVxx.eTypObj.IPe, CtrlCVxx.eTypObj.IPs, CtrlCVxx.eTypObj.IPw
                    PrintLine(1, "***********************" & obj.CvName & "**********************************")
                    If obj.Enabled Then
                        For j = 0 To obj.LinkNumber - 1 '' sur les 4 linkpoint lesquel sont connectés


                            If obj.LinkPoint(j)._Enable And obj.LinkPoint(j)._connected Then
                                PrintLine(1, "R_" & obj.CvName & " " & GND & " " & obj.LinkPoint(j)._LinkedToNode & " {1/" & obj.pumpingSpeed & "}") '' IP
                                PrintLine(1, "C_volume_" & obj.CvName & " " & GND & " " & obj.LinkPoint(j)._LinkedToNode & " " & obj.Volume) '' volume
                                PrintLine(1, ".SAVE " & obj.LinkPoint(j)._LinkedToNode)
                            End If




                        Next

                    End If
                Case CtrlCVxx.eTypObj.IPx
                    If obj.Enabled Then
                        PrintLine(1, "***********************" & obj.CvName & "**********************************") 'le model pour les IPx est différent dés ip 
                        PrintLine(1, "Rup_" & obj.CvName & " " & obj.LinkPoint(0)._LinkedToNode & " " & "middle_" & obj.CvName & " {1/" & obj.Conductance.C_in & "}") '' demi ip left
                        PrintLine(1, "Rmiddle_" & obj.CvName & " " & GND & " " & "middle_" & obj.CvName & " {1/" & obj.pumpingSpeed & "}") ''vitesse de pompage
                        PrintLine(1, "Rout_" & obj.CvName & " " & obj.LinkPoint(1)._LinkedToNode & " " & "middle_" & obj.CvName & " {1/" & obj.Conductance.C_out & "}") '' demi ip right
                        PrintLine(1, "C_volume_" & obj.CvName & " " & GND & " " & "middle_" & obj.CvName & " " & obj.Volume) '' volume
                        PrintLine(1, ".SAVE middle_" & obj.CvName)
                    End If
                Case CtrlCVxx.eTypObj.Start
                    If obj.Enabled Then
                        PrintLine(1, "***********************" & obj.CvName & "**********************************") 'model pour un start avec une resistance tres grande
                        PrintLine(1, "R1_" & obj.CvName & " " & GND & " " & obj.LinkPoint(1)._LinkedToNode & " 1000K") '' 
                        If obj.LinkPoint(1)._Enable And obj.LinkPoint(1)._connected Then PrintLine(1, ".SAVE " & obj.LinkPoint(1)._LinkedToNode)
                    End If
                Case CtrlCVxx.eTypObj.End_
                    If obj.Enabled Then
                        PrintLine(1, "***********************" & obj.CvName & "**********************************") 'model pour un start avec une resistance tres grande
                        PrintLine(1, "R1_" & obj.CvName & " " & obj.LinkPoint(0)._LinkedToNode & " " & GND & " 1000K") '' demi ip left
                        If obj.LinkPoint(0)._Enable And obj.LinkPoint(0)._connected Then PrintLine(1, ".SAVE " & obj.LinkPoint(0)._LinkedToNode)
                    End If

                Case CtrlCVxx.eTypObj.PEN, CtrlCVxx.eTypObj.PENe, CtrlCVxx.eTypObj.PENw, CtrlCVxx.eTypObj.PENs
                    If obj.Enabled Then
                        PrintLine(1, "***********************" & obj.CvName & "**********************************")

                        For j = 0 To obj.LinkNumber - 1 '' sur les 4 linkpoint lesquel sont connectés

                            If obj.LinkPoint(j)._Enable And obj.LinkPoint(j)._connected Then
                                PrintLine(1, "R_" & obj.CvName & " " & GND & " " & obj.LinkPoint(j)._LinkedToNode & " {1/" & obj.pumpingSpeed & "}") '
                                PrintLine(1, "C_volume_" & obj.CvName & " " & GND & " " & obj.LinkPoint(j)._LinkedToNode & " " & obj.Volume) '' volume
                                PrintLine(1, ".SAVE " & obj.LinkPoint(j)._LinkedToNode) 'pour avoir un point de mesure 
                            End If


                        Next
                    End If

                Case CtrlCVxx.eTypObj.Node, CtrlCVxx.eTypObj.NodeNE, CtrlCVxx.eTypObj.NodeNW, CtrlCVxx.eTypObj.NodeSE, CtrlCVxx.eTypObj.NodeWS, CtrlCVxx.eTypObj.NodeX
                    If obj.Enabled Then
                        PrintLine(1, "***********************" & obj.CvName & "**********************************")
                        PrintLine(1, "IQ_" & obj.CvName & " " & GND & " " & "middle_" & obj.CvName & " " & obj.Outgasing) '' degasage
                        PrintLine(1, "C_volume_" & obj.CvName & " " & GND & " " & "middle_" & obj.CvName & " " & obj.Volume) '' volume
                        If obj.NegActive Then PrintLine(1, "RNEG_" & obj.CvName & " " & GND & " " & "middle_" & obj.CvName & " {1/" & obj.NegPumpingSpeed & "}") '' si ya du neg
                        For j = 0 To obj.LinkNumber - 1 '' sur les 4 linkpoint lesquel sont connectés

                            Select Case j 'pour chaque node
                                Case 0 'in 
                                    If obj.LinkPoint(j)._Enable And obj.LinkPoint(j)._connected Then PrintLine(1, "Rw_" & obj.CvName & " " & obj.LinkPoint(0)._LinkedToNode & " " & "middle_" & obj.CvName & " {1/" & obj.Conductance.C_in & "}") '' link in west
                                Case 1 'out
                                    If obj.LinkPoint(j)._Enable And obj.LinkPoint(j)._connected Then PrintLine(1, "Re_" & obj.CvName & " " & obj.LinkPoint(1)._LinkedToNode & " " & "middle_" & obj.CvName & " {1/" & obj.Conductance.C_out & "}") '' link out east
                                Case 2 'top
                                    If obj.LinkPoint(j)._Enable And obj.LinkPoint(j)._connected Then PrintLine(1, "Rn_" & obj.CvName & " " & obj.LinkPoint(2)._LinkedToNode & " " & "middle_" & obj.CvName & " {1/" & obj.Conductance.C_up & "}") '' link top north
                                Case 3 'bot
                                    If obj.LinkPoint(j)._Enable And obj.LinkPoint(j)._connected Then PrintLine(1, "Rs_" & obj.CvName & " " & obj.LinkPoint(3)._LinkedToNode & " " & "middle_" & obj.CvName & " {1/" & obj.Conductance.C_down & "}") '' link bot south
                            End Select

                        Next
                    End If
                Case CtrlCVxx.eTypObj.GAZ
                    If obj.Enabled Then
                        PrintLine(1, "***********************" & obj.CvName & "**********************************")
                        If obj.Transien Then
                            PrintLine(1, "IQ_" & obj.CvName & " " & GND & " " & "middle_" & obj.CvName & " PULSE(" & obj.Pulse.V1 & " " & obj.Pulse.V2 & " " & obj.Pulse.TD & " " & obj.Pulse.Tr & " " & obj.Pulse.Tf & " " & obj.Pulse.PW & " " & obj.Pulse.Periode & ")") '' link point bot
                            PrintLine(1, ".TRAN 4 200 0 4")
                            PrintLine(1, ".PLOT " & """2CH4_3Pen3""")
                        Else
                            PrintLine(1, "IQ_" & obj.CvName & " " & GND & " " & "middle_" & obj.CvName & " " & obj.Leak)

                        End If

                        PrintLine(1, "Rbot_" & obj.CvName & " " & "middle_" & obj.CvName & " " & obj.LinkPoint(3)._LinkedToNode & " " & R0) '' link point bot


                    End If
            End Select
        Next

        PrintLine(1, ".OPTIONS filetype=ascii")
        PrintLine(1, ".OP")
        PrintLine(1, ".END")
        FileClose(1)


    End Sub
    Private Sub ConvertToSpice()
        Dim i, j As Integer
        Dim obj As CtrlCVxx

        Me.PathDataSave = VacSpicePath & "spicemodel.sp" '' SaveFileDialog1.FileName


        ' If Me.SaveFileDialog1.FileName <> Nothing Then
        File.Delete(PathDataSave)
        FileOpen(1, PathDataSave, OpenMode.Append)



        PrintLine(1, "VacuumGroupInst")
        PrintLine(1, "*SPICE Netlist generated by VacInstGroup on " & Now & " " & SaveFileDialog1.FileName)
        For i = 0 To IndexCtrlCV - 1
            obj = myCtrlCVxx(i)
            Select Case obj.TypeObj

                Case CtrlCVxx.eTypObj.CH, CtrlCVxx.eTypObj.CHpb, CtrlCVxx.eTypObj.CHph, CtrlCVxx.eTypObj.CHphb, CtrlCVxx.eTypObj.Volume1
                    If obj.Enabled Then

                        PrintLine(1, "***********************" & obj.CvName & "**********************************")
                        PrintLine(1, "*Schematic Netlist:")
                        PrintLine(1, "* Name :" & obj.CvName)
                        PrintLine(1, "* lenght(cm) :" & obj.longeur)
                        PrintLine(1, "* Diametre(cm) :" & obj.Diametre)
                        If obj.TypeObj = CtrlCVxx.eTypObj.Volume1 Then PrintLine(1, "* height(cm) :" & obj.volumey & "  width(cm) :" & obj.volumez)
                        If obj.NegActive Then PrintLine(1, "RNEG_" & obj.CvName & " " & GND & " " & "middle_" & obj.CvName & " {1/" & obj.NegPumpingSpeed & "}") '' si ya du neg
                        PrintLine(1, "IQ_" & obj.CvName & " " & GND & " " & "middle_" & obj.CvName & " " & obj.Outgasing) '' degasage
                        PrintLine(1, "Rup_" & obj.CvName & " " & obj.LinkPoint(0)._LinkedToNode & " " & "middle_" & obj.CvName & " {1/ " & obj.Conductance.C_in & "}") '' demi chambre left
                        PrintLine(1, "Rdown_" & obj.CvName & " " & "middle_" & obj.CvName & " " & obj.LinkPoint(1)._LinkedToNode & " {1/" & obj.Conductance.C_out & "}") '' demi chambre right
                        PrintLine(1, "C_volume_" & obj.CvName & " " & GND & " " & "middle_" & obj.CvName & " " & obj.Volume) '' volume
                        If obj.LinkPoint(2)._Enable And obj.LinkPoint(2)._connected Then PrintLine(1, "Rtop_" & obj.CvName & " " & "middle_" & obj.CvName & " " & obj.LinkPoint(2)._LinkedToNode & " " & R0) '' link point top
                        If obj.LinkPoint(3)._Enable And obj.LinkPoint(3)._connected Then PrintLine(1, "Rbot_" & obj.CvName & " " & "middle_" & obj.CvName & " " & obj.LinkPoint(3)._LinkedToNode & " " & R0) '' link point bot
                        ''PrintLine(1, ".SAVE " & obj.LinkPoint(0)._LinkedToNode)
                        '' PrintLine(1, ".SAVE middle_" & obj.CvName)

                    End If


                Case CtrlCVxx.eTypObj.IP, CtrlCVxx.eTypObj.IPe, CtrlCVxx.eTypObj.IPs, CtrlCVxx.eTypObj.IPw
                    PrintLine(1, "***********************" & obj.CvName & "**********************************")
                    If obj.Enabled Then
                        For j = 0 To obj.LinkNumber - 1 '' sur les 4 linkpoint lesquel sont connectés


                            If obj.LinkPoint(j)._Enable And obj.LinkPoint(j)._connected Then
                                PrintLine(1, "R_" & obj.CvName & " " & GND & " " & obj.LinkPoint(j)._LinkedToNode & " {1/" & obj.pumpingSpeed & "}") '' IP
                                PrintLine(1, "C_volume_" & obj.CvName & " " & GND & " " & obj.LinkPoint(j)._LinkedToNode & " " & obj.Volume) '' volume
                                PrintLine(1, ".SAVE " & obj.LinkPoint(j)._LinkedToNode)
                            End If




                        Next

                    End If
                Case CtrlCVxx.eTypObj.IPx
                    If obj.Enabled Then
                        PrintLine(1, "***********************" & obj.CvName & "**********************************") 'le model pour les IPx est différent dés ip 
                        PrintLine(1, "Rup_" & obj.CvName & " " & obj.LinkPoint(0)._LinkedToNode & " " & "middle_" & obj.CvName & " {1/" & obj.Conductance.C_in & "}") '' demi ip left
                        PrintLine(1, "Rmiddle_" & obj.CvName & " " & GND & " " & "middle_" & obj.CvName & " {1/" & obj.pumpingSpeed & "}") ''vitesse de pompage
                        PrintLine(1, "Rout_" & obj.CvName & " " & obj.LinkPoint(1)._LinkedToNode & " " & "middle_" & obj.CvName & " {1/" & obj.Conductance.C_out & "}") '' demi ip right
                        PrintLine(1, "C_volume_" & obj.CvName & " " & GND & " " & "middle_" & obj.CvName & " " & obj.Volume) '' volume
                        PrintLine(1, ".SAVE middle_" & obj.CvName)
                    End If
                Case CtrlCVxx.eTypObj.Start
                    If obj.Enabled Then
                        PrintLine(1, "***********************" & obj.CvName & "**********************************") 'model pour un start avec une resistance tres grande
                        PrintLine(1, "R1_" & obj.CvName & " " & GND & " " & obj.LinkPoint(1)._LinkedToNode & " 1000K") '' 
                        If obj.LinkPoint(1)._Enable And obj.LinkPoint(1)._connected Then PrintLine(1, ".SAVE " & obj.LinkPoint(1)._LinkedToNode)
                    End If
                Case CtrlCVxx.eTypObj.End_
                    If obj.Enabled Then
                        PrintLine(1, "***********************" & obj.CvName & "**********************************") 'model pour un start avec une resistance tres grande
                        PrintLine(1, "R1_" & obj.CvName & " " & obj.LinkPoint(0)._LinkedToNode & " " & GND & " 1000K") '' demi ip left
                        If obj.LinkPoint(0)._Enable And obj.LinkPoint(0)._connected Then PrintLine(1, ".SAVE " & obj.LinkPoint(0)._LinkedToNode)
                    End If

                Case CtrlCVxx.eTypObj.PEN, CtrlCVxx.eTypObj.PENe, CtrlCVxx.eTypObj.PENw, CtrlCVxx.eTypObj.PENs
                    If obj.Enabled Then
                        PrintLine(1, "***********************" & obj.CvName & "**********************************")

                        For j = 0 To obj.LinkNumber - 1 '' sur les 4 linkpoint lesquel sont connectés

                            If obj.LinkPoint(j)._Enable And obj.LinkPoint(j)._connected Then
                                PrintLine(1, "R_" & obj.CvName & " " & GND & " " & obj.LinkPoint(j)._LinkedToNode & " {1/" & obj.pumpingSpeed & "}") '
                                PrintLine(1, "C_volume_" & obj.CvName & " " & GND & " " & obj.LinkPoint(j)._LinkedToNode & " " & obj.Volume) '' volume
                                PrintLine(1, ".SAVE " & obj.LinkPoint(j)._LinkedToNode) 'pour avoir un point de mesure 
                            End If


                        Next
                    End If

                Case CtrlCVxx.eTypObj.Node, CtrlCVxx.eTypObj.NodeNE, CtrlCVxx.eTypObj.NodeNW, CtrlCVxx.eTypObj.NodeSE, CtrlCVxx.eTypObj.NodeWS, CtrlCVxx.eTypObj.NodeX
                    If obj.Enabled Then
                        PrintLine(1, "***********************" & obj.CvName & "**********************************")
                        PrintLine(1, "IQ_" & obj.CvName & " " & GND & " " & "middle_" & obj.CvName & " " & obj.Outgasing) '' degasage
                        PrintLine(1, "C_volume_" & obj.CvName & " " & GND & " " & "middle_" & obj.CvName & " " & obj.Volume) '' volume
                        If obj.NegActive Then PrintLine(1, "RNEG_" & obj.CvName & " " & GND & " " & "middle_" & obj.CvName & " {1/" & obj.NegPumpingSpeed & "}") '' si ya du neg
                        For j = 0 To obj.LinkNumber - 1 '' sur les 4 linkpoint lesquel sont connectés

                            Select Case j 'pour chaque node
                                Case 0 'in 
                                    If obj.LinkPoint(j)._Enable And obj.LinkPoint(j)._connected Then PrintLine(1, "Rw_" & obj.CvName & " " & obj.LinkPoint(0)._LinkedToNode & " " & "middle_" & obj.CvName & " {1/" & obj.Conductance.C_in & "}") '' link in west
                                Case 1 'out
                                    If obj.LinkPoint(j)._Enable And obj.LinkPoint(j)._connected Then PrintLine(1, "Re_" & obj.CvName & " " & obj.LinkPoint(1)._LinkedToNode & " " & "middle_" & obj.CvName & " {1/" & obj.Conductance.C_out & "}") '' link out east
                                Case 2 'top
                                    If obj.LinkPoint(j)._Enable And obj.LinkPoint(j)._connected Then PrintLine(1, "Rn_" & obj.CvName & " " & obj.LinkPoint(2)._LinkedToNode & " " & "middle_" & obj.CvName & " {1/" & obj.Conductance.C_up & "}") '' link top north
                                Case 3 'bot
                                    If obj.LinkPoint(j)._Enable And obj.LinkPoint(j)._connected Then PrintLine(1, "Rs_" & obj.CvName & " " & obj.LinkPoint(3)._LinkedToNode & " " & "middle_" & obj.CvName & " {1/" & obj.Conductance.C_down & "}") '' link bot south
                            End Select

                        Next
                    End If
                Case CtrlCVxx.eTypObj.GAZ
                    If obj.Enabled Then
                        PrintLine(1, "***********************" & obj.CvName & "**********************************")
                        If obj.Transien Then
                            PrintLine(1, "IQ_" & obj.CvName & " " & GND & " " & "middle_" & obj.CvName & " PULSE(" & obj.Pulse.V1 & " " & obj.Pulse.V2 & " " & obj.Pulse.TD & " " & obj.Pulse.Tr & " " & obj.Pulse.Tf & " " & obj.Pulse.PW & " " & obj.Pulse.Periode & ")") '' link point bot
                            PrintLine(1, ".TRAN 4 200 0 4")
                            ''  PrintLine(1, ".PLOT " & """2CH4_3Pen3""")
                        Else
                            PrintLine(1, "IQ_" & obj.CvName & " " & GND & " " & "middle_" & obj.CvName & " " & obj.Leak)

                        End If

                        PrintLine(1, "Rbot_" & obj.CvName & " " & "middle_" & obj.CvName & " " & obj.LinkPoint(3)._LinkedToNode & " " & R0) '' link point bot


                    End If
            End Select
        Next

        PrintLine(1, ".OPTIONS filetype=ascii")
        PrintLine(1, ".OP")
        PrintLine(1, ".END")
        FileClose(1)


    End Sub
    Private Sub AfficheLabelPressure(ByVal NbrSignal As Int16)
        Dim i, j As Integer
        Dim temp As String

        For i = 0 To NbrSignal - 1
            For j = 0 To IndexCtrlCV - 1

                If objName(i) = myCtrlCVxx(j).CvName.ToLower Then
                    MyCtrlLabel(NbrLabelPressure) = New CtrlLabel
                    MyCtrlLabel(NbrLabelPressure).CvPression = Format(Pressure(i), "0.0e-0")
                    MyCtrlLabel(NbrLabelPressure).Top = myCtrlCVxx(j).Top - 20
                    MyCtrlLabel(NbrLabelPressure).Left = myCtrlCVxx(j).Left + 10


                    Panel1.Controls.Add(MyCtrlLabel(NbrLabelPressure))
                    MyCtrlLabel(NbrLabelPressure).BringToFront()
                    NbrLabelPressure = NbrLabelPressure + 1
                End If

            Next
        Next
    End Sub
    Private Sub ClearLabelPressure()
        Dim i As Integer
        For i = 0 To NbrLabelPressure - 1
            MyCtrlLabel(i).Dispose()
        Next i

    End Sub
    Private Sub CallSpice()
        ''ngspice.exe -b t5.sp -r out.txt
        Dim oNbrLabelPressure As Integer

        oNbrLabelPressure = Shell(VacSpicePath & "ngspice.exe -b " & PathDataSave & " -r " & VacSpicePath & "out.txt")

    End Sub
    Private Sub OpenOutNetlist()
        Dim fnum As Int16
        Dim fname, temp As String

        fnum = 1
        fname = PathDataSave 'OpenFileDialog1.FileName


        FileOpen(fnum, fname, OpenMode.Input)


        ListBox3.Items.Add("888888888888888888888 Start 8888888888888888888888888888888888")
        Do

            ListBox3.Items.Add(LineInput(fnum))

        Loop Until EOF(1) 'fin du fichier
        ListBox3.Items.Add("88888888888888888888888 END 8888888888888888888888888888888888")
        FileClose(fnum)
    End Sub
    Private Sub OpenVacSimsFile()
        Dim fnum As Int16
        Dim fname, temp As String

        fnum = 1
        fname = FileVacSimsLoad 'OpenFileDialog1.FileName


        FileOpen(fnum, fname, OpenMode.Input)


        ListBox4.Items.Add("888888888888888888888 Start 8888888888888888888888888888888888")
        Do

            ListBox4.Items.Add(LineInput(fnum))

        Loop Until EOF(1) 'fin du fichier
        ListBox3.Items.Add("88888888888888888888888 END 8888888888888888888888888888888888")
        FileClose(fnum)
    End Sub
    Private Sub OpenOutSpice()
        Dim fnum, nmrLine, i, k, l, m As Int16
        Dim fname, temp As String
        Dim op, tr, op2, tr2 As Boolean

        fnum = 1
        fname = VacSpicePath & "out.txt" 'OpenFileDialog1.FileName


        FileOpen(fnum, fname, OpenMode.Input)


        ListBox2.Items.Add("888888888888888888888 Start 8888888888888888888888888888888888")
        Do
            nmrLine = nmrLine + 1
            temp = LineInput(fnum)
            ListBox2.Items.Add(temp)

            If Mid(temp, 1, 14) = "No. Variables:" And Not op Then
                NoVariablesOP = Mid(temp, 15)
                op = True
            ElseIf Mid(temp, 1, 14) = "No. Variables:" And op And Not tr Then
                NoVariablesTran = Mid(temp, 15)
                tr = True
            End If

            If Mid(temp, 1, 11) = "No. Points:" And Not op2 Then
                NoPointsOP = Mid(temp, 12)
                op2 = True
            ElseIf Mid(temp, 1, 11) = "No. Points:" And op2 And Not tr2 Then
                NoPointsTran = Mid(temp, 12)
                tr2 = True
                ReDim VariablesTran(NoPointsTran - 1, NoVariablesTran - 1)
            End If

            If nmrLine >= 8 And nmrLine < NoVariablesOP + 8 Then
                objName(i) = Mid(temp, InStr(temp, "_") + 2, InStr(temp, ")") - InStr(temp, "_") - 2)
                i = i + 1
            ElseIf nmrLine > 8 + NoVariablesOP And nmrLine <= (8 + NoVariablesOP * 2) + 1 Then
                Pressure(k) = Val(temp)
                k = k + 1
            ElseIf nmrLine > 15 + NoVariablesOP * 2 + NoVariablesTran + 1 Then
                Do
                    Do ' extraction des transient
                        VariablesTran(m, l) = Val(temp)
                        nmrLine = nmrLine + 1
                        temp = LineInput(fnum)
                        l = l + 1
                    Loop Until l = NoVariablesTran Or EOF(1)
                    m = m + 1
                    l = 0
                    nmrLine = nmrLine + 1

                    If m < NoPointsTran Then
                        VariablesTran(m, l) = Val(Mid(temp, InStr(temp, vbTab)))
                        l = 1
                        temp = LineInput(fnum)
                    End If

                Loop Until m = NoPointsTran Or EOF(1)
            End If

        Loop Until EOF(1) 'fin du fichier
        ListBox2.Items.Add("88888888888888888888888 END 8888888888888888888888888888888888")
        FileClose(fnum)
        AfficheLabelPressure(NoVariablesOP)

    End Sub
    Private Sub NewDraw()
        Dim i As Integer



        For i = 0 To IndexCtrlCV - 1
            myCtrlCVxx(i).Visible = False

            myCtrlCVxx(i).Dispose()

        Next
        Erase myCtrlCVxx
        ReDim myCtrlCVxx(100)
        IndexCtrlCV = 0
        IndexCHClose = 0
        Erase indexFamily
        ReDim indexFamily(10)
        Panel1.Refresh()
        ObjDeleted = 0
        ClearLabelPressure()
    End Sub
    Private Sub loadDraw()
        '' Dim myStream As Stream = Nothing
        ''  Dim openFileDialog1 As New OpenFileDialog()
        Dim fnum, numberline As Int16
        Dim fname, temp As String
        Dim objNbr As Int16 = 0
        ObjDeleted = 0
        fnum = 1
        fname = OpenFileDialog1.FileName

        OpenFileDialog1.Filter = "(*.vac)|*.vac"
        OpenFileDialog1.ShowDialog()
        Me.PathDataSave = OpenFileDialog1.FileName
        FileVacSimsLoad = PathDataSave
        FileOpen(fnum, PathDataSave, OpenMode.Input)

        temp = LineInput(fnum) 'date
        temp = LineInput(fnum) 'max obj
        ReDim mydata(Mid(temp, InStr(temp, "[") + 1, (InStr(temp, "]") - InStr(temp, "[")) - 1), 50)

        Do

            temp = LineInput(fnum)
            If temp <> Nothing And Mid(temp, 10, 1) <> "=" And Mid(temp, 10, 1) <> "-" Then
                If Mid(temp, 1, 2) = "->" Then
                    objNbr = Mid(temp, InStr(temp, "[") + 1, (InStr(temp, "]") - InStr(temp, "[")) - 1)
                    numberline = 0
                End If
                mydata(objNbr, numberline) = Mid(temp, InStr(temp, "[") + 1, (InStr(temp, "]") - InStr(temp, "[")) - 1)
                numberline = numberline + 1

            End If
        Loop Until EOF(1) 'fin du fichier
        FileClose(fnum)

        OpenVacSimsFile()

        rebuildDraw()


    End Sub
    Private Sub ExportFileTransien(ByVal Nbrcolonne As Integer, ByVal NbrLigne As Integer)

        Dim ligne, colonne As Integer
        Dim maligne As String = ""

        SaveFileDialog1.Filter = "(*.xls)|*.xls"
        SaveFileDialog1.ShowDialog()
        Me.PathDataSave = SaveFileDialog1.FileName
        'FileVacSimsLoad = PathDataSave

        If Me.SaveFileDialog1.FileName <> Nothing Then

            FileOpen(1, PathDataSave, OpenMode.Append)
            maligne = "Time" & vbTab
            Do
                maligne = maligne & objName(colonne) & vbTab
                colonne = colonne + 1
            Loop Until colonne = Nbrcolonne
            PrintLine(1, maligne)
            maligne = ""
            colonne = 0

            Do
                Do
                    maligne = maligne & VariablesTran(ligne, colonne) & vbTab
                    colonne = colonne + 1
                Loop Until colonne = Nbrcolonne
                PrintLine(1, maligne)
                maligne = ""
                colonne = 0
                ligne = ligne + 1
            Loop Until ligne = NbrLigne

        End If

        FileClose(1)
    End Sub
    Private Sub SaveDraw()
        'pour sauvegarder les données d'un dessin
        Dim i, j, indexlinkpoint As Int16


        SaveFileDialog1.Filter = "(*.vac)|*.vac"
        SaveFileDialog1.ShowDialog()
        Me.PathDataSave = SaveFileDialog1.FileName
        FileVacSimsLoad = PathDataSave

        If Me.SaveFileDialog1.FileName <> Nothing Then
            FileOpen(1, PathDataSave, OpenMode.Append)



            PrintLine(1, "file created : " & Now)
            PrintLine(1, "nbr obj created : [" & (IndexCtrlCV - 1) - ObjDeleted & "]")
            For i = 0 To IndexCtrlCV - 1
                If myCtrlCVxx(i).Enabled Then
                    PrintLine(1, "-> IndexCtrlCV: [" & j & "]")
                    PrintLine(1, "CvName: [" & myCtrlCVxx(i).CvName & "]")
                    PrintLine(1, "IndexFamily: [" & myCtrlCVxx(i).IndexFamily & "]")
                    PrintLine(1, "TypeObj: [" & myCtrlCVxx(i).TypeObj & "]")
                    PrintLine(1, "Location.X: [" & myCtrlCVxx(i).Location.X & "]")
                    PrintLine(1, "Location.Y: [" & myCtrlCVxx(i).Location.Y & "]")
                    PrintLine(1, "longeur: [" & myCtrlCVxx(i).longeur & "]")

                    PrintLine(1, "X_in: [" & myCtrlCVxx(i).X_in & "]") ' X1 pour les nodes
                    PrintLine(1, "X_out: [" & myCtrlCVxx(i).X_out & "]") 'X2
                    PrintLine(1, "Y_up: [" & myCtrlCVxx(i).Y_up & "]") 'Y1
                    PrintLine(1, "Y_down: [" & myCtrlCVxx(i).Y_down & "]") 'Y2

                    PrintLine(1, "Diametre: [" & myCtrlCVxx(i).Diametre & "]")
                    PrintLine(1, "Family: [" & myCtrlCVxx(i).Family & "]")
                    PrintLine(1, "OutgasingType: [" & myCtrlCVxx(i).OutgasingType & "]")
                    PrintLine(1, "Conductance: [" & myCtrlCVxx(i).Conductance.Conductance & "]")

                    For indexlinkpoint = 0 To 3
                        PrintLine(1, "      ---------------------------------------------------------------")
                        PrintLine(1, "      indexlinkpoint: [" & indexlinkpoint & "]")
                        PrintLine(1, "      _connected: [" & myCtrlCVxx(i).LinkPoint(indexlinkpoint)._connected & "]")
                        PrintLine(1, "      _enable: [" & myCtrlCVxx(i).LinkPoint(indexlinkpoint)._Enable & "]")
                        PrintLine(1, "      _linkedToObj: [" & myCtrlCVxx(i).LinkPoint(indexlinkpoint)._linkedToObj & "]")
                        PrintLine(1, "      _linkedToObjName: [" & myCtrlCVxx(i).LinkPoint(indexlinkpoint)._linkedToObjName & "]")
                        PrintLine(1, "      _linkedToPoint: [" & myCtrlCVxx(i).LinkPoint(indexlinkpoint)._linkedToPoint & "]")
                        PrintLine(1, "      _LinkedToNode: [" & myCtrlCVxx(i).LinkPoint(indexlinkpoint)._LinkedToNode & "]")
                        PrintLine(1, "      _Point.X: [" & myCtrlCVxx(i).LinkPoint(indexlinkpoint)._Point.X & "]")
                        PrintLine(1, "      _Point.Y: [" & myCtrlCVxx(i).LinkPoint(indexlinkpoint)._Point.Y & "]")

                    Next

                    PrintLine(1, "====================================================================")
                    j = j + 1



                End If
            Next







            FileClose(1)
            OpenVacSimsFile()
        End If

    End Sub

#End Region

#Region "function et sub avec Handler"
    Private Sub RemovedObj(ByVal indexObj As Int16)

        myCtrlCVxx(indexObj).Dispose()
        ObjDeleted = ObjDeleted + 1
        '' For i = indexObj To IndexCtrlCV - 2

        ''myCtrlCVxx(indexObj) = myCtrlCVxx(indexObj + 1)
        '' Next
        ' IndexCtrlCV = IndexCtrlCV - 1
        ''myCtrlCVxx(indexObj).Visible = False
        myCtrlCVxx(indexObj).Enabled = False
    End Sub
    Private Sub CloserCH(ByVal indexObj As Int16)

        CHClose = findPointsToDrawline2(indexObj)


    End Sub
    Private Sub InfoObjClicked(ByVal index As Int16)
        '' affiche les infos sur les CV 
        '' AfficheMSG("Object selected " & vbTab & index)
        '' ListBox1.Items.Add("obj clicked :" & index)
        PictureBoxObjInfo.Image = myCtrlCVxx(index).ObjImage
        Select Case myCtrlCVxx(index).Family
            Case CtrlCVxx.eFamily.CH
                GroupBoxCH.Visible = True
                GroupBoxNode.Visible = False
                GroupBoxPump.Visible = False
                GroupBoxVolume.Visible = False
                GroupBoxGaz.Visible = False
                GroupBoxNodeStd.Visible = False
                GroupBoxOutGasing.Visible = True
                GroupBoxPenning.Visible = False
            Case CtrlCVxx.eFamily.VOLUME
                GroupBoxOutGasing.Visible = True
                GroupBoxCH.Visible = False
                GroupBoxNode.Visible = False
                GroupBoxPump.Visible = False
                GroupBoxVolume.Visible = True
                GroupBoxGaz.Visible = False
                GroupBoxNodeStd.Visible = False
                GroupBoxPenning.Visible = False
            Case CtrlCVxx.eFamily.IP
                GroupBoxCH.Visible = False
                GroupBoxNode.Visible = False
                GroupBoxPump.Visible = True
                GroupBoxVolume.Visible = False
                GroupBoxGaz.Visible = False
                GroupBoxNodeStd.Visible = False
                GroupBoxOutGasing.Visible = False
                GroupBoxPenning.Visible = False
            Case CtrlCVxx.eFamily.NODE
                Select Case myCtrlCVxx(index).TypeObj
                    Case CtrlCVxx.eTypObj.Node, CtrlCVxx.eTypObj.NodeNE, CtrlCVxx.eTypObj.NodeNW, CtrlCVxx.eTypObj.NodeSE, CtrlCVxx.eTypObj.NodeWS
                        GroupBoxCH.Visible = False
                        GroupBoxNode.Visible = False
                        GroupBoxPump.Visible = False
                        GroupBoxVolume.Visible = False
                        GroupBoxGaz.Visible = False
                        GroupBoxNodeStd.Visible = True
                        GroupBoxOutGasing.Visible = False
                        GroupBoxPenning.Visible = False
                    Case CtrlCVxx.eTypObj.NodeX
                        GroupBoxCH.Visible = False
                        GroupBoxNode.Visible = True
                        GroupBoxPump.Visible = False
                        GroupBoxVolume.Visible = False
                        GroupBoxGaz.Visible = False
                        GroupBoxNodeStd.Visible = False
                        GroupBoxOutGasing.Visible = True
                        GroupBoxPenning.Visible = False
                End Select

            Case CtrlCVxx.eFamily.PEN
                GroupBoxCH.Visible = False
                GroupBoxNode.Visible = False
                GroupBoxPump.Visible = False
                GroupBoxVolume.Visible = False
                GroupBoxGaz.Visible = False
                GroupBoxNodeStd.Visible = False
                GroupBoxOutGasing.Visible = False
                GroupBoxPenning.Visible = True
            Case CtrlCVxx.eFamily.START
                GroupBoxCH.Visible = False
                GroupBoxNode.Visible = False
                GroupBoxPump.Visible = False
                GroupBoxVolume.Visible = False
                GroupBoxGaz.Visible = False
                GroupBoxNodeStd.Visible = False
                GroupBoxOutGasing.Visible = False
                GroupBoxPenning.Visible = False
            Case CtrlCVxx.eFamily.END_
                GroupBoxCH.Visible = False
                GroupBoxNode.Visible = False
                GroupBoxPump.Visible = False
                GroupBoxVolume.Visible = False
                GroupBoxGaz.Visible = False
                GroupBoxNodeStd.Visible = False
                GroupBoxOutGasing.Visible = False
                GroupBoxPenning.Visible = False
            Case CtrlCVxx.eFamily.GAZ
                GroupBoxCH.Visible = False
                GroupBoxNode.Visible = False
                GroupBoxPump.Visible = False
                GroupBoxVolume.Visible = False
                GroupBoxGaz.Visible = True
                GroupBoxNodeStd.Visible = False
                GroupBoxOutGasing.Visible = False
                GroupBoxPenning.Visible = False

        End Select

        LabelType.Text = myCtrlCVxx(index).TypeObjString
        LabelIndex.Text = index
        CVClicked = index
        TextBoxX1.Text = myCtrlCVxx(index).X_in
        TextBoxX2.Text = myCtrlCVxx(index).X_out
        TextBoxY1.Text = myCtrlCVxx(index).Y_up
        TextBoxY2.Text = myCtrlCVxx(index).Y_down

        TextBox1.Text = Format(myCtrlCVxx(index).Diametre, "0000")
        TextBoxLenghtCv.Text = Format(myCtrlCVxx(index).longeur, "0000")
        TextBoxDiametreCV.Text = Format(myCtrlCVxx(index).Diametre, "0000")
        LabelSurface.Text = Format(myCtrlCVxx(index).Surface, "0.00")
        LabelVolume.Text = Format(myCtrlCVxx(index).Volume, "0.0000")
        LabelConductance.Text = Format(myCtrlCVxx(index).Conductance.Conductance, "0.00")
        LabelFamily.Text = myCtrlCVxx(index).Family
        TextBoxCvName.Text = myCtrlCVxx(index).CvName
        LabelOutgasing.Text = Format(myCtrlCVxx(index).Outgasing, "0.E00")
        TextBoxIpPumpingSpeed.Text = Format(myCtrlCVxx(index).pumpingSpeed, "000")
        TextBoxPenningPumpingSpeed.Text = myCtrlCVxx(index).pumpingSpeed
        If myCtrlCVxx(index).NegActive Then
            LabelNegPumpingSpeed.Text = Format(myCtrlCVxx(index).NegPumpingSpeed, "#.00")
        Else
            LabelNegPumpingSpeed.Text = 0
        End If

        TextBoxVolumeY.Text = Format(myCtrlCVxx(index).volumey, "0000")
        TextBoxVolumeZ.Text = Format(myCtrlCVxx(index).volumez, "0000")

        ''CtrlInfo1.model = myCtrlCVxx(index).TypeObj

        Select Case myCtrlCVxx(index).OutgasingType
            Case CtrlCVxx.eBaked.baked
                RadioButton1.Checked = False
                RadioButton2.Checked = True
                RadioButton3.Checked = False

            Case CtrlCVxx.eBaked.Unbaked
                RadioButton1.Checked = True
                RadioButton2.Checked = False
                RadioButton3.Checked = False
            Case CtrlCVxx.eBaked.other
                RadioButton1.Checked = False
                RadioButton2.Checked = False
                RadioButton3.Checked = True

        End Select


        ''CtrlInfo1.Visible = True


        '' Label19.Text = myCtrlCVxx(index).li.ObjNumberMooving
        '' Label14.Text = findPointsToDrawline2.LinkNumberMooving
        '' LabelObjNameFixe.Text = findPointsToDrawline2.ObjNameParent
        '' LabelObjNameMooving.Text = findPointsToDrawline2.objNameChild
    End Sub

    Private Sub StickCH(ByVal release As Boolean, ByVal indexCH As Int16)


        'quand le bouton de la souris est relachée alors je colle la derniere CH a la suite de la CH la plus proche, la CH liée avec une ligne
        If CHClose.Find And release Then
            'debug

            ''obj mooving
            myCtrlCVxx(indexCH).Location = CHClose.StickLocation                                                        'reajuste la position de l'obj laché
            myCtrlCVxx(indexCH).LinkPoint(CHClose.LinkNumberMooving)._linkedToObj = CHClose.ObjNumberFixe               'numero de l'obj connecté au linkpoint
            myCtrlCVxx(indexCH).LinkPoint(CHClose.LinkNumberMooving)._linkedToObjName = CHClose.ObjNameParent           'nom de l'obj parent connecté
            myCtrlCVxx(indexCH).LinkPoint(CHClose.LinkNumberMooving)._connected = True
            myCtrlCVxx(indexCH).LinkPoint(CHClose.LinkNumberMooving)._linkedToPoint = CHClose.LinkNumberFixe            ' numero du linkpoint 
            myCtrlCVxx(indexCH).reajustLinkPointsPosition = CHClose.StickLocation
            myCtrlCVxx(indexCH).LinkPoint(CHClose.LinkNumberMooving)._LinkedToNode = CHClose.LinkNumberFixe & CHClose.ObjNameParent & "_" & CHClose.LinkNumberMooving & myCtrlCVxx(indexCH).CvName ''"Node_" & indexCH

            '' obj fixe 
            myCtrlCVxx(CHClose.ObjNumberFixe).LinkPoint(CHClose.LinkNumberFixe)._connected = True
            myCtrlCVxx(CHClose.ObjNumberFixe).LinkPoint(CHClose.LinkNumberFixe)._linkedToObj = CHClose.ObjNumberMooving
            myCtrlCVxx(CHClose.ObjNumberFixe).LinkPoint(CHClose.LinkNumberFixe)._linkedToObjName = CHClose.objNameChild
            myCtrlCVxx(CHClose.ObjNumberFixe).LinkPoint(CHClose.LinkNumberFixe)._linkedToPoint = CHClose.LinkNumberMooving
            myCtrlCVxx(CHClose.ObjNumberFixe).LinkPoint(CHClose.LinkNumberFixe)._LinkedToNode = CHClose.LinkNumberFixe & CHClose.ObjNameParent & "_" & CHClose.LinkNumberMooving & myCtrlCVxx(indexCH).CvName ''"Node_" & indexCH

        End If
        ClearPanel()
    End Sub
#End Region

#Region "mouse events"
    Private Sub IndexObjCopied(ByVal indexobjcopied As Int16)
        ObjCopied = indexobjcopied
    End Sub
    Private Sub Paste(ByVal indexObjToBeCopied As Int16)
        'quand un obj est copier je le past .
        'add obj by default CH
        Dim mypos As New Point(120, 120)

        myCtrlCVxx(IndexCtrlCV) = New CtrlCVxx()
        '' myCtrlCVxx(IndexCtrlCV) = myCtrlCVxx(indexObjToBeCopied) ''copy l'integralité de l obj
        myCtrlCVxx(IndexCtrlCV).Index = IndexCtrlCV  'assignation du numero d'index a l'obj.
        myCtrlCVxx(IndexCtrlCV).IndexFamily = myCtrlCVxx(indexObjToBeCopied).IndexFamily



        myCtrlCVxx(IndexCtrlCV).Regime = myCtrlCVxx(indexObjToBeCopied).Regime 'definition du regime 
        myCtrlCVxx(IndexCtrlCV).Location = mypos
        myCtrlCVxx(IndexCtrlCV).longeur = myCtrlCVxx(indexObjToBeCopied).longeur   'longeur de la chambre en m
        myCtrlCVxx(IndexCtrlCV).Diametre = myCtrlCVxx(indexObjToBeCopied).Diametre    'diametre de la chambre en mm
        myCtrlCVxx(IndexCtrlCV).TypeObj = myCtrlCVxx(indexObjToBeCopied).TypeObj '' CtrlCVxx.eTypObj.CH 'definition du type de l'objet par defaut CH
        myCtrlCVxx(IndexCtrlCV).OutgasingType = myCtrlCVxx(indexObjToBeCopied).OutgasingType
        ''myCtrlCVxx(IndexCtrlCV).CvName=

        Panel1.Controls.Add(myCtrlCVxx(IndexCtrlCV))
        IndexCtrlCV = IndexCtrlCV + 1
    End Sub

    Private Sub mnuOptionsSnapToGrid_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles mnuOptionsSnapToGrid.Click
        m_SnapToGrid = Not m_SnapToGrid
        mnuOptionsSnapToGrid.Checked = m_SnapToGrid
    End Sub



    ' Continue drawing.
    Private Sub picCanvas_MouseMove(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Panel1.MouseMove
        m_X2 = e.X
        m_Y2 = e.Y
        SnapToGrid(m_X2, m_Y2)

        ' Show the mouse position on the rulers.
        ShowMousePosition(m_X2, m_Y2)
        Label1.Text = e.X
        Label2.Text = e.Y
        ' Redraw.

    End Sub




#End Region



#Region "saisies au clavier pour les calcules"

    Private Sub TextBoxVolumeX_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBoxVolumeX.KeyPress
        If e.KeyChar = ChrW(Keys.Enter) Then
            myCtrlCVxx(LabelIndex.Text).longeur = TextBoxVolumeX.Text
            LabelOutgasing.Text = myCtrlCVxx(LabelIndex.Text).Outgasing

            InfoObjClicked(LabelIndex.Text)

        End If
    End Sub
    Private Sub TextBoxLenghtCv_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBoxLenghtCv.KeyPress
        If e.KeyChar = ChrW(Keys.Enter) Then
            myCtrlCVxx(LabelIndex.Text).longeur = TextBoxLenghtCv.Text
            LabelOutgasing.Text = myCtrlCVxx(LabelIndex.Text).Outgasing
            InfoObjClicked(LabelIndex.Text)
            Beep()

        End If
    End Sub




    Private Sub TextBoxDiametreCV_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBoxDiametreCV.KeyPress
        If e.KeyChar = ChrW(Keys.Enter) Then
            myCtrlCVxx(LabelIndex.Text).Diametre = TextBoxDiametreCV.Text
            LabelOutgasing.Text = myCtrlCVxx(LabelIndex.Text).Outgasing
            Beep()
            InfoObjClicked(LabelIndex.Text)
        End If
    End Sub

    Private Sub TextBox1_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBoxCvName.KeyPress
        If e.KeyChar = ChrW(Keys.Enter) Then
            myCtrlCVxx(LabelIndex.Text).CvName = TextBoxCvName.Text
            Beep()
            InfoObjClicked(LabelIndex.Text)
        End If
    End Sub



    Private Sub RadioButton1_CheckedChanged(sender As Object, e As EventArgs) Handles RadioButton1.Click
        'unbaked outgasing 3e-10

        Try
            myCtrlCVxx(LabelIndex.Text).OutgasingType = CtrlCVxx.eBaked.Unbaked
            LabelOutgasingFactor.Text = myCtrlCVxx(LabelIndex.Text).OutgasingFactor
            InfoObjClicked(LabelIndex.Text)
        Catch ex As Exception

        End Try

    End Sub

    Private Sub RadioButton2_CheckedChanged(sender As Object, e As EventArgs) Handles RadioButton2.Click
        'baked outgasing 1e-11

        Try


            myCtrlCVxx(LabelIndex.Text).OutgasingType = CtrlCVxx.eBaked.baked
            LabelOutgasingFactor.Text = myCtrlCVxx(LabelIndex.Text).OutgasingFactor
            InfoObjClicked(LabelIndex.Text)
        Catch ex As Exception

        End Try

    End Sub
    Private Sub RadioButton3_CheckedChanged(sender As Object, e As EventArgs) Handles RadioButton3.Click
        'other outgasing factor
        Try
            TextBoxOutGasing.Enabled = RadioButton3.Checked
            myCtrlCVxx(LabelIndex.Text).OutgasingFactor = TextBoxOutGasing.Text
            myCtrlCVxx(LabelIndex.Text).OutgasingType = CtrlCVxx.eBaked.other
            InfoObjClicked(LabelIndex.Text)
        Catch ex As Exception

        End Try

    End Sub


    Private Sub TextBoxOutGasing_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBoxOutGasing.KeyPress
        If e.KeyChar = ChrW(Keys.Enter) Then
            myCtrlCVxx(LabelIndex.Text).OutgasingFactor = TextBoxOutGasing.Text
            myCtrlCVxx(LabelIndex.Text).OutgasingType = CtrlCVxx.eBaked.other
            InfoObjClicked(LabelIndex.Text)

            Beep()

        End If
    End Sub

    Private Sub TextBoxX1_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBoxX1.KeyPress
        If e.KeyChar = ChrW(Keys.Enter) Then
            myCtrlCVxx(LabelIndex.Text).X_in = TextBoxX1.Text
            InfoObjClicked(LabelIndex.Text)
            Beep()

        End If
    End Sub

    Private Sub TextBoxX2_TextChanged(sender As Object, e As KeyPressEventArgs) Handles TextBoxX2.KeyPress
        If e.KeyChar = ChrW(Keys.Enter) Then
            myCtrlCVxx(LabelIndex.Text).X_out = TextBoxX2.Text
            InfoObjClicked(LabelIndex.Text)
            Beep()

        End If
    End Sub


    Private Sub TextBoxY2_TextChanged(sender As Object, e As KeyPressEventArgs) Handles TextBoxY2.KeyPress
        If e.KeyChar = ChrW(Keys.Enter) Then
            myCtrlCVxx(LabelIndex.Text).Y_down = TextBoxY2.Text
            InfoObjClicked(LabelIndex.Text)
            Beep()

        End If
    End Sub
    Private Sub TextBoxY1_TextChanged(sender As Object, e As KeyPressEventArgs) Handles TextBoxY1.KeyPress
        If e.KeyChar = ChrW(Keys.Enter) Then
            myCtrlCVxx(LabelIndex.Text).Y_up = TextBoxY1.Text
            InfoObjClicked(LabelIndex.Text)
            Beep()

        End If
    End Sub
    Private Sub TextBox1_KeyPress1(sender As Object, e As KeyPressEventArgs) Handles TextBox1.KeyPress  'diamtre 
        If e.KeyChar = ChrW(Keys.Enter) Then
            myCtrlCVxx(LabelIndex.Text).Diametre = TextBox1.Text
            InfoObjClicked(LabelIndex.Text)
            Beep()

        End If
    End Sub


    Private Sub TextBoxIpPumpingSpeed_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBoxIpPumpingSpeed.KeyPress
        If e.KeyChar = ChrW(Keys.Enter) Then
            myCtrlCVxx(LabelIndex.Text).pumpingSpeed = TextBoxIpPumpingSpeed.Text
            InfoObjClicked(LabelIndex.Text)
        End If
    End Sub

    Private Sub TextBoxIPdiametre_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBoxIPdiametre.KeyPress
        If e.KeyChar = ChrW(Keys.Enter) Then
            myCtrlCVxx(LabelIndex.Text).Diametre = TextBoxIPdiametre.Text
            InfoObjClicked(LabelIndex.Text)
        End If
    End Sub








    Private Sub TextBoxVolumeY_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBoxVolumeY.KeyPress
        If e.KeyChar = ChrW(Keys.Enter) Then
            myCtrlCVxx(LabelIndex.Text).volumey = TextBoxVolumeY.Text
            InfoObjClicked(LabelIndex.Text)

        End If
    End Sub







    Private Sub TextBoxVolumeZ_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBoxVolumeZ.KeyPress
        If e.KeyChar = ChrW(Keys.Enter) Then
            myCtrlCVxx(LabelIndex.Text).volumez = TextBoxVolumeZ.Text
            InfoObjClicked(LabelIndex.Text)

        End If
    End Sub

    Private Sub TextBox8_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox8.KeyPress
        If e.KeyChar = ChrW(Keys.Enter) Then
            myCtrlCVxx(LabelIndex.Text).Leak = TextBox8.Text
            InfoObjClicked(LabelIndex.Text)

        End If
    End Sub

    Private Sub TextBoxPenningPumpingSpeed_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBoxPenningPumpingSpeed.KeyPress
        If e.KeyChar = ChrW(Keys.Enter) Then
            myCtrlCVxx(LabelIndex.Text).pumpingSpeed = TextBoxPenningPumpingSpeed.Text
            InfoObjClicked(LabelIndex.Text)

        End If
    End Sub


#End Region


#Region "dessine les lignes PAINT"
    Private Sub Panel1_Paint(sender As Object, e As PaintEventArgs) Handles Panel1.Paint
        '' redessine la ligne entre les CH car a chaque deplacement d'un objet la ligne est dessiné
        '' Panel1.Invalidate() 'efface les squelettes de lignes....
        If CHClose.Find Then

            e.Graphics.DrawLine(m_PenOldLine, CHClose.MyPointFixeclose.X, CHClose.MyPointFixeclose.Y, CHClose.MypointMoovingClose.X, CHClose.MypointMoovingClose.Y)

        End If

    End Sub
    Private Sub ClearPanel()
        Dim g As Graphics = Panel1.CreateGraphics
        g.Clear(Color.White)
        g.Dispose()
    End Sub
#End Region


#Region "menu"
    Private Sub SaveDrawToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SaveDrawToolStripMenuItem.Click
        SaveDraw()
    End Sub

    Private Sub OpenSaveDrawToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles OpenSaveDrawToolStripMenuItem.Click
        loadDraw()

    End Sub

    Private Sub NewToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles NewToolStripMenuItem.Click
        NewDraw()

    End Sub
    Private Sub PastToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles PastToolStripMenuItem.Click
        Paste(ObjCopied)
    End Sub

    Private Sub ClearPressureToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ClearPressureToolStripMenuItem.Click
        ClearLabelPressure()

    End Sub


    Private Sub ExportToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ExportToolStripMenuItem.Click
        ExportFileTransien(NoVariablesTran, NoPointsTran)
    End Sub

#End Region

#Region "buttons"

    Private Sub ButtonNorth_Click(sender As Object, e As EventArgs) Handles ButtonNorth.Click
        Select Case ObjSelection
            Case CtrlCVxx.eFamily.CH
                AddObj(CtrlCVxx.eTypObj.CHph, CtrlCVxx.eFamily.CH)

            Case CtrlCVxx.eFamily.IP
                AddObj(CtrlCVxx.eTypObj.IP, CtrlCVxx.eFamily.IP)

            Case CtrlCVxx.eFamily.NODE
                AddObj(CtrlCVxx.eTypObj.NodeNE, CtrlCVxx.eFamily.NODE)

            Case CtrlCVxx.eFamily.PEN
                AddObj(CtrlCVxx.eTypObj.PEN, CtrlCVxx.eFamily.PEN)



        End Select
    End Sub

    Private Sub ButtonEast_Click(sender As Object, e As EventArgs) Handles ButtonEast.Click

        Select Case ObjSelection
            Case CtrlCVxx.eFamily.CH
                AddObj(CtrlCVxx.eTypObj.CH, CtrlCVxx.eFamily.CH)

            Case CtrlCVxx.eFamily.IP
                AddObj(CtrlCVxx.eTypObj.IPe, CtrlCVxx.eFamily.IP)

            Case CtrlCVxx.eFamily.NODE
                AddObj(CtrlCVxx.eTypObj.NodeSE, CtrlCVxx.eFamily.NODE)

            Case CtrlCVxx.eFamily.PEN
                AddObj(CtrlCVxx.eTypObj.PENe, CtrlCVxx.eFamily.PEN)




        End Select
    End Sub

    Private Sub ButtonSouth_Click(sender As Object, e As EventArgs) Handles ButtonSouth.Click
        Select Case ObjSelection
            Case CtrlCVxx.eFamily.CH
                AddObj(CtrlCVxx.eTypObj.CHpb, CtrlCVxx.eFamily.CH)

            Case CtrlCVxx.eFamily.IP
                AddObj(CtrlCVxx.eTypObj.IPs, CtrlCVxx.eFamily.IP)

            Case CtrlCVxx.eFamily.NODE
                AddObj(CtrlCVxx.eTypObj.NodeWS, CtrlCVxx.eFamily.NODE)

            Case CtrlCVxx.eFamily.PEN
                AddObj(CtrlCVxx.eTypObj.PENs, CtrlCVxx.eFamily.PEN)



        End Select
    End Sub

    Private Sub ButtonEveryWhere_Click(sender As Object, e As EventArgs) Handles ButtonEveryWhere.Click
        Select Case ObjSelection
            Case CtrlCVxx.eFamily.CH
                AddObj(CtrlCVxx.eTypObj.CHphb, CtrlCVxx.eFamily.CH)

            Case CtrlCVxx.eFamily.IP
                AddObj(CtrlCVxx.eTypObj.IPx, CtrlCVxx.eFamily.IP)

            Case CtrlCVxx.eFamily.NODE
                AddObj(CtrlCVxx.eTypObj.NodeX, CtrlCVxx.eFamily.NODE)



        End Select
    End Sub

    Private Sub Button7_Click(sender As Object, e As EventArgs) Handles Button7.Click
        If NbrLabelPressure > 0 Then ClearLabelPressure()
        ConvertToSpice()
        Sleep(1000)
        CallSpice()
        Sleep(1000)
        OpenOutNetlist()

        Sleep(1000)
        OpenOutSpice()
    End Sub


    Private Sub Button10_Click(sender As Object, e As EventArgs)
        CallSpice()
    End Sub



#End Region


#Region " listbox"
    Private Sub ListBox2_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ListBox2.DoubleClick
        ListBox2.Items.Clear()

    End Sub

    Private Sub ListBox3_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ListBox3.DoubleClick
        ListBox3.Items.Clear()
    End Sub

#End Region


#Region "radiobutton"
    Private Sub RadioButtonLeak_CheckedChanged(sender As Object, e As EventArgs) Handles RadioButtonLeak.CheckedChanged
        Dim mysize1 As New Size(345, 68)
        Dim mysize2 As New Size(345, 277)
        Try


            GroupBox3.Enabled = Not RadioButtonLeak.Checked
            GroupBox3.Visible = Not RadioButtonLeak.Checked
            If RadioButtonLeak.Checked Then
                GroupBoxGaz.Size = mysize1
                myCtrlCVxx(LabelIndex.Text).Transien = False
            Else
                GroupBoxGaz.Size = mysize2
                myCtrlCVxx(LabelIndex.Text).Transien = True
            End If

        Catch ex As Exception

        End Try
    End Sub

#End Region












    Protected Overrides Sub Finalize()
        MyBase.Finalize()
    End Sub







    Private Sub Button10_Click_1(sender As Object, e As EventArgs) Handles Button10.Click

        Mypulse.V1 = TextBoxV1.Text
        Mypulse.V2 = TextBoxV2.Text
        Mypulse.Tr = TextBoxTr.Text
        Mypulse.Tf = TextBoxTf.Text
        Mypulse.TD = TextBoxTD.Text
        Mypulse.PW = TextBoxPW.Text
        Mypulse.Periode = TextBoxPeriod.Text

        myCtrlCVxx(LabelIndex.Text).Pulse = Mypulse

    End Sub


    
End Class



