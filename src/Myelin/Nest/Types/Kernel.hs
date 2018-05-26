module Myelin.Nest.Types.Kernel where

-- | Representation of the Nest Kernel status
data Status = Status {
    _t_max :: Float,
    _t_min :: Float,
    _data_path :: String,
    _data_prefix :: String,
    _dict_miss_is_error :: Bool,
    _grng_seed :: Int,
    _initial_connector_capacity :: Int,
    _large_connector_growth_factor :: Float,
    _large_connector_limit :: Int,
    _local_num_threads :: Int,
    _local_spike_counter :: Int,
    _max_delay :: Float,
    _min_delay :: Float,
    _ms_per_tic :: Float,
    _num_processes :: Int,
    _off_grid_spiking :: Bool,
    _overwrite_files :: Bool,
    _print_time :: Bool,
    _receive_buffer_size:: Int,
    _resolution :: Float,
    _send_buffer_size :: Int,
    _tics_per_ms :: Float,
    _tics_per_step :: Int,
    _time :: Float,
    _time_collocate :: Float,
    _time_communicate :: Float
} deriving (Read, Show, Eq, Ord)
