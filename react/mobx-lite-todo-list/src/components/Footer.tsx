import React from 'react'

interface Props {
  remaining: number;
  total: number;
}

export const Footer: React.FC<Props> = props => {
  return (
    <p data-testid="footer">
      { props.remaining } / { props.total } left
    </p>
  )
}