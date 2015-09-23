%-----------------------------------------------------------------------------%
% Extra built-in constraints for the FD/LP backend
%-----------------------------------------------------------------------------%

annotation lp;

% Bounds changes on 'x' cause corresponding bounds changes on 'y'.
%
predicate channel_fd_int_to_lp_float(var int: x, var float: y);

% Compute the upper and lower bound of 'x' and, if found, channel
% as bounds changes on 'y'.
%
predicate channel_lp_float_to_fd_int(var float: x, var int: y);
