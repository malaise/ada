with Intra_Dictio, Nodes, Fight_Mng;
package body Init_Mng is

  procedure Start is
  begin
    Fight_Mng.Start (Status.Init, 2.0, 
      (Nodes.Many_Master_Master => Status.Starting,
       Nodes.Many_Master_Slave  => Status.Starting,
       Nodes.One_Master_Master  => Status.Slave,
       Nodes.One_Master_Slave   => Status.Slave,
       Nodes.All_Init_Master    => Status.Master,
       Nodes.All_Init_Slave     => Status.Slave,
       Nodes.No_Master_Master   => Status.Master,
       Nodes.No_Master_Slave    => Status.Starting) );

  end Start;

end Init_Mng;

